defmodule Mix.Tasks.Compile.Rustler do
  use Mix.Task

  alias Rustler.Compiler.{Messages, Rustup}

  @recursive true

  def run(_args) do
    config = Mix.Project.config()
    crates = Keyword.get(config, :rustler_crates) || raise_missing_crates()

    File.mkdir_p!(priv_dir())

    Enum.map(crates, &compile_crate/1)

    # Workaround for a mix problem. We should REALLY get this fixed properly.
    _ = symlink_or_copy(config,
      Path.expand("priv"),
      Path.join(Mix.Project.app_path(config), "priv"))
  end


  defp priv_dir, do: "priv/native"

  def compile_crate({id, config}) do
    crate_path = Keyword.get(config, :path)
    build_mode = Keyword.get(config, :mode, :release)

    Mix.shell.info "Compiling NIF crate #{inspect id} (#{crate_path})..."

    compile_command =
      make_base_command(Keyword.get(config, :cargo, :system))
      |> make_no_default_features_flag(Keyword.get(config, :default_features, true))
      |> make_features_flag(Keyword.get(config, :features, []))
      |> make_build_mode_flag(build_mode)
      |> make_platform_hacks(:os.type())

    crate_full_path = Path.expand(crate_path, File.cwd!)
    target_dir = Path.join([Mix.Project.build_path(), "rustler_crates",
                            Atom.to_string(id)])

    cargo_data = check_crate_env(crate_full_path)
    lib_name = Rustler.TomlParser.get_table_val(cargo_data, ["lib"], "name")

    if lib_name == nil do
      throw_error({:cargo_no_library, crate_path})
    end

    [cmd_bin | args] = compile_command

    compile_return = System.cmd(cmd_bin, args, [
      cd: crate_full_path,
      stderr_to_stdout: true,
      env: [{"CARGO_TARGET_DIR", target_dir} | Keyword.get(config, :env, [])],
      into: IO.stream(:stdio, :line),
    ])

    case compile_return do
      {_, 0} -> nil
      {_, code} -> raise "Rust NIF compile error (rustc exit code #{code})"
    end

    {src_file, dst_file} = make_lib_name(lib_name)
    compiled_lib = Path.join([target_dir, Atom.to_string(build_mode), src_file])
    destination_lib = Path.join(priv_dir(), dst_file)

    File.cp!(compiled_lib, destination_lib)
  end

  defp make_base_command(:system), do: ["cargo", "rustc"]
  defp make_base_command({:bin, path}), do: [path, "rustc"]
  defp make_base_command({:rustup, version}) do
    if Rustup.version == :none do
      throw_error(:rustup_not_installed)
    end

    unless Rustup.version_installed?(version) do
      throw_error({:rust_version_not_installed, version})
    end

    ["rustup", "run", version, "cargo", "rustc"]
  end

  defp make_platform_hacks(args, {:unix, :darwin}) do
    # Fix for https://github.com/hansihe/Rustler/issues/12
    args ++ ["--", "--codegen", "link-args=-flat_namespace -undefined suppress"]
  end
  defp make_platform_hacks(args, _), do: args

  defp make_no_default_features_flag(args, true), do: args ++ []
  defp make_no_default_features_flag(args, false), do: args ++ ["--no-default-features"]

  defp make_features_flag(args, []), do: args ++ []
  defp make_features_flag(args, flags), do: args ++ ["--features", Enum.join(flags, ",")]

  defp make_build_mode_flag(args, :release), do: args ++ ["--release"]
  defp make_build_mode_flag(args, :debug), do: args ++ []

  def make_lib_name(base_name) do
    case :os.type do
      {:win32, _} -> {"#{base_name}.dll", "lib#{base_name}.dll"}
      {:unix, :darwin} -> {"lib#{base_name}.dylib", "lib#{base_name}.so"}
      {:unix, :linux} -> {"lib#{base_name}.so", "lib#{base_name}.so"}
      # {:unix, _} -> Assume .so? Is this a unix thing?
    end
  end

  def throw_error(error_descr) do
    Mix.shell.error Messages.message(error_descr)
    raise "Compilation error"
  end

  def check_crate_env(crate) do
    unless File.dir?(crate) do
      throw_error({:nonexistent_crate_directory, crate})
    end

    cargo_data =
      case File.read("#{crate}/Cargo.toml") do
        {:error, :enoent} ->
          throw_error({:cargo_toml_not_found, crate})
        {:ok, text} ->
          Rustler.TomlParser.parse(text)
      end

    cargo_data
  end

  defp raise_missing_crates do
    Mix.raise """
    Missing required :rustler_crates option in mix.exs.
    """
  end

  # https://github.com/elixir-lang/elixir/blob/b13404e913fff70e080c08c2da3dbd5c41793b54/lib/mix/lib/mix/project.ex#L553-L562
  defp symlink_or_copy(config, source, target) do
    if config[:build_embedded] do
      if File.exists?(source) do
        File.rm_rf!(target)
        File.cp_r!(source, target)
      end
    else
      Mix.Utils.symlink_or_copy(source, target)
    end
  end
end
