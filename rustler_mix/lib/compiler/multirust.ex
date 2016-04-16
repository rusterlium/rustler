defmodule Rustler.Compiler.Multirust do

  def version do
    multirust_exec = System.find_executable("multirust")
    if multirust_exec == nil do
      :none
    else
      {version, 0} = System.cmd("multirust", ["--version"])
      {:ok, version}
    end
  end

  def rust_versions do
    {versions_raw, 0} = System.cmd("multirust", ["list-toolchains"])
    versions_raw
    |> String.strip(?\n)
    |> String.split("\n")
  end

  def install_toolchain(version) do
    {_resp, 0} = System.cmd("multirust", ["update", version],
                            into: IO.stream(:stdio, :line))
  end

  def compile_with(path, version, args \\ [], env \\ [], into \\ "") do
    System.cmd("multirust", ["run", version, "cargo", "build" | args],
               cd: path, into: into, env: env)
  end

end
