defmodule Rustler.Compiler do
  @moduledoc false

  alias Rustler.Compiler.{Config, Messages, Rustup}

  @doc false
  def compile_crate(otp_app, config, opts) do
    config = Config.from(otp_app, config, opts)

    unless config.skip_compilation? do
      crate_full_path = Path.expand(config.path, File.cwd!())

      File.mkdir_p!(priv_dir())

      Mix.shell().info("Compiling crate #{config.crate} in #{config.mode} mode (#{config.path})")

      [cmd | args] =
        make_base_command(config.cargo)
        |> make_no_default_features_flag(config.default_features)
        |> make_features_flag(config.features)
        |> make_target_flag(config.target)
        |> make_build_mode_flag(config.mode)

      ensure_platform_requirements!(crate_full_path, config, :os.type())

      compile_result =
        System.cmd(cmd, args,
          cd: crate_full_path,
          stderr_to_stdout: true,
          env: [{"CARGO_TARGET_DIR", config.target_dir} | config.env],
          into: IO.stream(:stdio, :line)
        )

      case compile_result do
        {_, 0} -> :ok
        {_, code} -> raise "Rust NIF compile error (rustc exit code #{code})"
      end

      handle_artifacts(crate_full_path, config)
      # See #326: Ensure that the libraries are copied into the correct subdirectory
      # in `_build`.
      Mix.Project.build_structure()
    end

    config
  end

  defp make_base_command(:system), do: ["cargo", "rustc"]
  defp make_base_command({:system, channel}), do: ["cargo", channel, "rustc"]
  defp make_base_command({:bin, path}), do: [path, "rustc"]

  defp make_base_command({:rustup, version}) do
    if Rustup.version() == :none do
      throw_error(:rustup_not_installed)
    end

    unless Rustup.version_installed?(version) do
      throw_error({:rust_version_not_installed, version})
    end

    ["rustup", "run", version, "cargo", "rustc"]
  end

  defp ensure_platform_requirements!(_, _, _), do: :ok

  defp make_no_default_features_flag(args, true), do: args
  defp make_no_default_features_flag(args, false), do: args ++ ["--no-default-features"]

  defp make_features_flag(args, []), do: args
  defp make_features_flag(args, flags), do: args ++ ["--features", Enum.join(flags, ",")]

  defp make_target_flag(args, target) when is_binary(target), do: args ++ ["--target=#{target}"]
  defp make_target_flag(args, _), do: args

  defp make_build_mode_flag(args, :release), do: args ++ ["--release"]
  defp make_build_mode_flag(args, :debug), do: args

  defp get_target_os_type(nil) do
    :os.type()
  end

  defp get_target_os_type(target) when is_binary(target) do
    os_type =
      [
        %{pattern: ~r/.*-linux.*/, os_type: {:unix, :linux}},
        %{pattern: ~r/.*-windows.*/, os_type: {:win32, :nt}},
        %{pattern: ~r/.*-apple.*/, os_type: {:unix, :darwin}},
        %{pattern: ~r/.*-freebsd.*/, os_type: {:unix, :freebsd}},
        %{pattern: ~r/.*-netbsd.*/, os_type: {:unix, :netbsd}},
        %{pattern: ~r/.*-openbsd.*/, os_type: {:unix, :openbsd}},
        %{pattern: ~r/.*-solaris.*/, os_type: {:unix, :solaris}}
      ]
      |> Enum.find(&Regex.match?(&1.pattern, target))

    if os_type do
      os_type.os_type
    else
      throw_error(
        {:unknown_target,
         "#{target} is not in the support list yet. Please report it on https://github.com/rusterlium/rustler/issues."}
      )
    end
  end

  defp handle_artifacts(path, config) do
    toml = toml_data(path)
    target = config.target
    names = get_name(toml, :lib) ++ get_name(toml, :bin)

    output_dir =
      if is_binary(target) do
        Path.join([target, Atom.to_string(config.mode)])
      else
        Atom.to_string(config.mode)
      end

    Enum.each(names, fn {name, type} ->
      {src_file, dst_file} = make_file_names(name, type, target)
      compiled_lib = Path.join([config.target_dir, output_dir, src_file])
      destination_lib = Path.join(priv_dir(), dst_file)

      # If the file exists already, we delete it first. This is to ensure that another
      # process, which might have the library dynamically linked in, does not generate
      # a segfault. By deleting it first, we ensure that the copy operation below does
      # not write into the existing file.
      File.rm(destination_lib)
      File.cp!(compiled_lib, destination_lib)
    end)
  end

  defp get_name(toml, section) do
    case toml[to_string(section)] do
      nil -> []
      values when is_map(values) -> [{values["name"], section}]
      values when is_list(values) -> Enum.map(values, &{&1["name"], section})
    end
  end

  defp get_suffix(target, :lib) do
    case get_target_os_type(target) do
      {:win32, _} -> "dll"
      {:unix, :darwin} -> "dylib"
      {:unix, _} -> "so"
    end
  end

  defp get_suffix(target, :bin) do
    case get_target_os_type(target) do
      {:win32, _} -> "exe"
      {:unix, _} -> ""
    end
  end

  defp make_file_names(base_name, :lib, target) do
    suffix = get_suffix(target, :lib)

    case get_target_os_type(target) do
      {:win32, _} -> {"#{base_name}.#{suffix}", "lib#{base_name}.dll"}
      {:unix, :darwin} -> {"lib#{base_name}.#{suffix}", "lib#{base_name}.so"}
      {:unix, _} -> {"lib#{base_name}.#{suffix}", "lib#{base_name}.so"}
    end
  end

  defp make_file_names(base_name, :bin, target) do
    suffix = get_suffix(target, :bin)

    case get_target_os_type(target) do
      {:win32, _} -> {"#{base_name}.#{suffix}", "#{base_name}.exe"}
      {:unix, _} -> {base_name, base_name}
    end
  end

  defp throw_error(error_descr) do
    Mix.shell().error(Messages.message(error_descr))
    raise "Compilation error"
  end

  defp toml_data(path) do
    unless File.dir?(path) do
      throw_error({:nonexistent_crate_directory, path})
    end

    case File.read("#{path}/Cargo.toml") do
      {:error, :enoent} ->
        throw_error({:cargo_toml_not_found, path})

      {:ok, text} ->
        Toml.decode!(text)
    end
  end

  defp priv_dir, do: "priv/native"
end
