defmodule Rustler.Compiler.Messages do
  @moduledoc false

  def message(:rustup_not_installed) do
    """
    Rustup could not be found on your machine.
    It is required for building NIF crates automatically.

    Follow the instructions in https://www.rustup.rs/ to install it.
    """
  end

  def message({:nonexistent_crate_directory, crate}) do
    """
    Directory '#{crate}' could not be found.
    One of the crate directories specified for compilation does not exist.
    """
  end

  def message({:cargo_toml_not_found, crate}) do
    """
    No 'Cargo.toml' could be found in the '#{crate}' directory.
    Are you sure it is a Rust crate?
    """
  end

  def message(:no_crates_property) do
    """
    No NIF crates listed in project definition.
    Add some crate directories under :rustler_crates in the mix.exs project definition to compile them.

    Example:
    rustler_crates: ["/native/my_nif_crate"]
    """
  end

  def message(:no_config) do
    """
    Add your crate to the 'rustler_crates' attribute in the project function.
    """
  end

  def message({:differing_versions, crate, rustler_version, codegen_version}) do
    """
    The '#{crate}' crate should have the same rustler and rustler_codegen version in its Cargo.toml:
    rustler version: #{rustler_version}
    rustler_codegen version: #{codegen_version}
    """
  end

  def message({:unsupported_rustler_version, crate, supported, version}) do
    """
    The crate '#{crate}' is using an unsupported version of rustler and rustler_codegen.

    Should be: '#{supported}''
    Is: '#{version}'

    Note: The version numbers need to match exactly, including space and equals.
    """
  end

  def message({:rust_version_not_installed, needed_version}) do
    """
    Required Rust toolchain version #{needed_version} is not installed.
    It can be installed by running 'rustup update #{needed_version}'.
    """
  end

  def message({:no_rustler_deps, crate, version}) do
    """
    No Rustler dependencies found in Cargo.toml for crate #{crate}.
    Please make sure the following is in the dependencies section of your Cargo.toml file:

    > rustler = #{version}
    > rustler_codegen = #{version}

    Note: You should already have this if you made your project with the project generator.
    """
  end

  def message({:cargo_no_name, crate}) do
    """
    No library or binary with name listed in Cargo.toml of crate '#{crate}'.
    """
  end

  def message({:unsupported_nif_version, version}) do
    """
    Your current version of Erlang is on NIF version '#{version}'.
    Rustler currently supports versions #{inspect(Rustler.nif_versions())}.

    Go open an issue about this on github!
    """
  end
end
