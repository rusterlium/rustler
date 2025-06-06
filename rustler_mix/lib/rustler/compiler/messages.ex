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

  def message({:unknown_target, target}) do
    """
    #{target} is not in the support list yet. Please report it on https://github.com/rusterlium/rustler/issues."
    """
  end
end
