defmodule Mix.Tasks.Rustler do
  use Mix.Task

  @shortdoc "Prints Rustler help information"

  @moduledoc """
  Prints Rustler tasks and their information

      mix rustler
  """

  @doc false
  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)

    case args do
      [] -> general()
      _ ->
        Mix.raise "Invalid arguments, expected: mix rustler"
    end
  end

  defp general() do
    Application.ensure_all_started(:rustler)
    Mix.shell.info(
      """
      Rustler v#{Application.spec(:rustler, :vsn)}
      Safe Rust bridge for creating Erlang NIF functions.\n
      Available tasks:
      """
    )
    Mix.Tasks.Help.run(["--search", "rustler."])
  end
end
