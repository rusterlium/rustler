defmodule Mix.Tasks.Compile.Rustler do
  use Mix.Task

  alias Rustler.Compiler.Messages
  alias Rustler.Compiler.Multirust

  def throw_error(error_descr) do
    Mix.shell.error Messages.message(error_descr)
    raise "Compilation error"
  end

  def run(_args) do
    project = Mix.Project.config
    crates = project[:rustler_crates]

    if Multirust.version == :none do
      throw_error(:multirust_not_installed)
    end

    rust_versions = Multirust.rust_versions
    unless Rustler.rust_version in rust_versions do
      throw_error({:rust_version_not_installed, Rustler.rust_version})
    end

    nif_version = :erlang.system_info(:nif_version)
    unless nif_version in Rustler.nif_versions do
      throw_error({:unsupported_nif_version, nif_version})
    end

    File.mkdir_p!(Rustler.nif_lib_dir)

    is_release = System.get_env("RUSTLER_DEBUG") != "true"
    compile_crates(crates, is_release)
  end

  def compile_crates(nil, _), do: throw_error(:no_crates_property)
  def compile_crates(crates, release) do
    Enum.map(crates, &compile_crate(&1, release))
  end

  def compile_crate(crate, release) do
    cargo_data = check_crate_env(crate)

    lib_name = Rustler.TomlParser.get_table_val(cargo_data, ["lib"], "name")
    if lib_name == nil do
      throw_error({:cargo_no_library, crate})
    end

    compiler_flags = if release, do: ["--release"], else: []
    compile_flavor = if release, do: "release", else: "debug"

    target_dir = "#{Rustler.nif_lib_dir}/#{crate}_target"

    Mix.shell.info "Compiling NIF crate '#{crate}' (#{compile_flavor})..."
    case Multirust.compile_with(crate, Rustler.rust_version, compiler_flags,
                                [{"CARGO_TARGET_DIR", target_dir}],
                                IO.stream(:stdio, :line)) do
      {_, 0} -> nil
      _ -> raise "Compile error"
    end

    # TODO: Make build flavor specific
    Enum.map(Path.wildcard("#{target_dir}/#{compile_flavor}/lib#{lib_name}.*"), fn
      path ->
        file_name = platform_library_name(Path.basename(path))
        File.cp!(Path.absname(path), "#{Rustler.nif_lib_dir}/#{file_name}")
    end)
  end

  def platform_library_name(base_name) do
    ext = Path.extname(base_name)
    case ext do
      # For macs we need to rename the .dylib so .so for the BEAM to load it.
      # See issue #3.
      ".dylib" -> Path.rootname(base_name) <> ".so"
      _ -> base_name
    end
  end

  def check_crate_env(crate) do
    unless File.dir?(crate) do
      Mix.shell.error Messages.message(:no_crates_property_message)
    end

    cargo_data =
      case File.read("#{crate}/Cargo.toml") do
        {:error, :enoent} ->
          throw_error({:cargo_toml_not_found, crate})
        {:ok, text} ->
          Rustler.TomlParser.parse(text)
      end

    rustler_version = get_cargo_dep_info(cargo_data, "rustler")
    rustler_codegen_version = get_cargo_dep_info(cargo_data, "rustler_codegen")

    validate_versions(crate, rustler_version, rustler_codegen_version)

    cargo_data
  end

  def validate_versions(crate, rustler, codegen)
  when rustler == nil or codegen == nil do
    throw_error({:no_rustler_deps, crate, Rustler.rustler_version})
  end
  def validate_versions(_, %{version: nil}, %{version: nil}) do
    # Development, no validation
  end
  def validate_versions(crate, %{version: rustler_v}, %{version: codegen_v})
  when rustler_v != codegen_v do
    throw_error({:differing_versions, crate, rustler_v, codegen_v})
  end
  def validate_versions(crate, %{version: version}, %{version: version}) do
    unless version == "=#{Rustler.rustler_version}" do
      throw_error({:unsupported_rustler_version, crate,
                   "=#{Rustler.rustler_version}", version})
    end
  end

  def get_cargo_dep_info(data, dep_name) do
    table_path = ["dependencies", dep_name]
    simple_key = Rustler.TomlParser.get_table_val(data, ["dependencies"], dep_name)
    table = Rustler.TomlParser.get_table_vals(data, ["dependencies", dep_name])

    cond do
      simple_key != nil ->
        %{version: simple_key}
      table != nil ->
        %{
          version: Rustler.TomlParser.get_keys_key(table, "version"),
          path: Rustler.TomlParser.get_keys_key(table, "path"),
          git: Rustler.TomlParser.get_keys_key(table, "git"),
        }
      true ->
        nil
    end
  end

end
