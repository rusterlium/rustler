defmodule Rustler.Compiler do
  @moduledoc false

  alias Rustler.Compiler.{Config, Messages, Rustup}

  @doc false
  def compile_crate(otp_app, config, opts) do
    config = Config.from(otp_app, config, opts)

    if !config.skip_compilation? do
      File.mkdir_p!(priv_dir())

      Mix.shell().info("Compiling crate #{config.crate} in #{config.mode} mode (#{config.path})")
      do_compile(config)
      Mix.Project.build_structure()

      # See #326: Ensure that the libraries are copied into the correct subdirectory
      # in `_build`.
      missing = Enum.reject(expected_files(config), &File.exists?/1)

      if missing != [] do
        Mix.shell().info("Expected files do not exist: #{inspect(missing)}, rebuilding")

        do_compile(config, true)
        Mix.Project.build_structure()
      end

      missing = Enum.reject(expected_files(config), &File.exists?/1)

      if missing != [] do
        raise "Expected files do still not exist: #{inspect(missing)}\nPlease verify your configuration"
      end
    end

    config
  end

  defp do_compile(config, recompile \\ false) do
    [cmd | args] =
      rustc(config.cargo)
      |> build_flags(config)
      |> make_build_mode_flag(config.mode)

    args =
      if recompile do
        # HACK: Cargo does not allow forcing a recompilation, but we need the
        # compiler-artifact messages, so force a recompilation via a "config
        # change"
        args ++ ["--cfg", "build_#{System.system_time(:millisecond)}"]
      else
        args
      end

    compile_result =
      System.cmd(cmd, args, env: config.env, into: %Rustler.BuildResults{}, lines: 1024)

    artifacts =
      case compile_result do
        {build_results, 0} -> build_results.artifacts
        {_, code} -> raise "Rust NIF compile error (rustc exit code #{code})"
      end

    handle_artifacts(artifacts, config)
  end

  defp build_flags(args, config) do
    args
    |> make_package_flag(config.crate)
    |> make_no_default_features_flag(config.default_features)
    |> make_features_flag(config.features)
    |> make_target_flag(config.target)
  end

  defp rustc(cargo) do
    make_base_command(cargo) ++ ["rustc", "--message-format=json-render-diagnostics", "--quiet"]
  end

  defp make_base_command(:system), do: ["cargo"]
  defp make_base_command({:system, channel}), do: ["cargo", channel]
  defp make_base_command({:bin, path}), do: [path]

  defp make_base_command({:rustup, version}) do
    if Rustup.version() == :none do
      throw_error(:rustup_not_installed)
    end

    if !Rustup.version_installed?(version) do
      throw_error({:rust_version_not_installed, version})
    end

    ["rustup", "run", version, "cargo"]
  end

  defp make_package_flag(args, crate), do: args ++ ["-p", "#{crate}"]

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
      throw_error({:unknown_target, target})
    end
  end

  defp handle_artifacts(artifacts, config) do
    target = config.target

    if artifacts == [] do
      throw_error(:no_artifacts)
    end

    Enum.each(artifacts, fn artifact = %Rustler.BuildArtifact{} ->
      if artifact.kind == :lib or artifact.kind == :bin do
        dst_file = output_file(artifact.name, artifact.kind, target)
        destination_lib = Path.join(priv_dir(), dst_file)

        # If the file exists already, we delete it first. This is to ensure that another
        # process, which might have the library dynamically linked in, does not generate
        # a segfault. By deleting it first, we ensure that the copy operation below does
        # not write into the existing file.
        Mix.shell().info("Copying #{artifact.filename} to #{destination_lib}")
        File.rm(destination_lib)
        File.cp!(artifact.filename, destination_lib)
      end
    end)
  end

  defp expected_files(config) do
    lib = if config.lib, do: [output_file(config.crate, :lib, config.target)], else: []
    bins = for bin <- config.binaries, do: output_file(bin, :bin, config.target)

    for filename <- lib ++ bins do
      Path.join(priv_dir(), filename)
    end
  end

  defp output_file(base_name, kind, target) do
    "#{base_name}#{suffix(kind, target)}"
  end

  defp suffix(:lib, target) do
    case get_target_os_type(target) do
      {:win32, _} -> ".dll"
      {:unix, :darwin} -> ".so"
      {:unix, _} -> ".so"
    end
  end

  defp suffix(:bin, target) do
    case get_target_os_type(target) do
      {:win32, _} -> ".exe"
      {:unix, _} -> ""
    end
  end

  defp throw_error(error_descr) do
    Mix.shell().error(Messages.message(error_descr))
    raise "Compilation error"
  end

  defp priv_dir, do: "priv/native"
end
