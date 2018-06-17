defmodule Mix.Tasks.Rustler.Test do
  use Mix.Task

  @shortdoc "Run cargo tests from Elixir"

  @moduledoc """
  This task runs `cargo test` for NIF crates configured to be used with Elixir.

  By running Rust tests directly from within BEAM, users can also write tests in Rust
  using types defined by Rustler. Usually, this is not possibly, as those types often
  only work properly when the executable is dynamically linked in by BEAM.

  Usage:
  mix rustler.test

  ## Arguments

  * `--crate`: Run tests for specific crates. This option can be used multiple times.

  """

  def run(argv) do
    {opts, _} = OptionParser.parse!(argv, strict: [crate: :keep, all: :boolean])

    crates = get_crates(opts)
    cwd = System.cwd()
    homedir = System.user_home()

    for {crate, kw} <- crates do
      Mix.shell().info("Running tests for #{crate}")
      manifest_path = cwd |> Path.join(kw[:path]) |> Path.join("Cargo.toml")
      Rustler.CargoTestRunner.run_tests(cwd, homedir, manifest_path)
    end
  end

  defp get_crates(opts) do
    config = Mix.Project.config()
    all_crates = config[:rustler_crates]

    selected_crates =
      with selected when is_list(selected) <- Enum.filter(opts, &(elem(&1, 0) == :crate)) do
        selected |> Enum.unzip() |> elem(1)
      end

    if selected_crates != [] do
      all_crates_set = all_crates |> Enum.unzip() |> elem(0) |> MapSet.new()
      selected_crates_set = selected_crates |> Enum.map(&String.to_atom/1) |> MapSet.new()

      if MapSet.subset?(selected_crates_set, all_crates_set) do
        Enum.filter(all_crates, fn {crate, _} -> crate in selected_crates_set end)
      else
        diff =
          selected_crates_set
          |> MapSet.difference(all_crates_set)
          |> MapSet.to_list()

        Mix.shell().error("Tried to select crates missing in mix.exs: #{inspect(diff)}")
        throw(:wrong_crates)
      end
    else
      all_crates
    end
  end
end
