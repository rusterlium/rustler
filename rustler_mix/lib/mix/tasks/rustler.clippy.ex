defmodule Mix.Tasks.Rustler.Clippy do
  use Mix.Task

  @shortdoc "Run `cargo clippy` inside Rustler projects"
  @moduledoc """
  Runs `cargo clippy` within a Rustler project's crates.

  Usage:

  ```
  mix rustler.clippy [--crate <name>] | --all
  ```
  """

  @option_schema [
    strict: [
      all: :boolean,
      crate: :keep
    ]
  ]

  def run(argv) do
    case OptionParser.parse(argv, @option_schema) do
      {[], [], []} ->
        Mix.Task.run("help", ["rustler.clippy"])

      {switches, _, _} ->
        cond do
          Keyword.has_key?(switches, :all) && Keyword.has_key?(switches, :crate) ->
            Mix.shell().error("Please provide either `--all` or `--crate` option, not both.")
            Mix.Task.run("help", ["rustler.clippy"])

          Keyword.has_key?(switches, :all) ->
            find_all_crates()
            |> clippy_all()

          true ->
            switches
            |> Keyword.get_values(:crate)
            |> clippy_all()
        end
    end
  end

  defp crate_path do
    File.cwd!()
    |> Path.join("native")
  end

  defp find_all_crates do
    crate_path()
    |> Path.join("*/Cargo.toml")
    |> Path.wildcard()
    |> Enum.map(&Path.dirname/1)
    |> Enum.map(&Path.basename/1)
  end

  defp clippy_all(crates) do
    crate_path = crate_path()

    status =
      Enum.reduce(crates, 0, fn crate, status ->
        Mix.shell().info("Running clippy for `#{crate}` crate:")

        exit_code =
          Mix.shell().cmd("cargo clippy",
            cd: Path.join(crate_path, crate),
            env: %{"RUSTFLAGS" => "-Dwarnings"}
          )

        if exit_code > 0 do
          Mix.shell().error("Clippy failed for crate `#{crate}`")
        end

        status + exit_code
      end)

    if status > 0 do
      System.at_exit(fn _ -> exit({:shutdown, status}) end)
    end
  end
end
