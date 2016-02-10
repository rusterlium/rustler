defmodule Mix.Tasks.Rustler.Build do
  use Mix.Task

  @switches [release: :boolean, cargo_args: :string]

  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: @switches)

    Mix.Task.run "rustler.check"

    build(opts)
  end

  def build(opts) do
    Mix.Shell.IO.cmd("cargo build")
  end

end
