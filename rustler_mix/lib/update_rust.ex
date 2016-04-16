defmodule Mix.Tasks.Rustler.UpdateRust do
  use Mix.Task

  def run(_args) do
    Rustler.Compiler.Multirust.install_toolchain(Rustler.rust_version)
  end
end
