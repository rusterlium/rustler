defmodule Rustler.Compiler.Rustup do
  @moduledoc false

  def rustup_binary, do: System.get_env("RUSTUP_BINARY") || "rustup"

  def version do
    multirust_exec = System.find_executable(rustup_binary())

    if multirust_exec == nil do
      :none
    else
      {version, 0} = System.cmd(rustup_binary(), ["--version"])
      {:ok, version}
    end
  end

  def version_installed?(version) do
    {_resp, return} = System.cmd(rustup_binary(), ["run", version, "rustc", "--version"])

    case return do
      0 -> true
      _ -> false
    end
  end

  # def rust_versions do
  #  {versions_raw, 0} = System.cmd("rustup", ["list-toolchains"])
  #  versions_raw
  #  |> String.strip(?\n)
  #  |> String.split("\n")
  # end

  def install_toolchain(version) do
    {_resp, 0} = System.cmd(rustup_binary(), ["install", version], into: IO.stream(:stdio, :line))
  end

  def compile_with(path, version, args \\ [], env \\ [], into \\ "") do
    System.cmd(rustup_binary(), ["run", version, "cargo", "build" | args],
      cd: path,
      into: into,
      env: env
    )
  end
end
