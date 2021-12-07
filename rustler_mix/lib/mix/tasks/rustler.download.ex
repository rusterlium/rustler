defmodule Mix.Tasks.Rustler.Download do
  @shortdoc "Download precompiled NIFs and build checksums"

  @moduledoc """
  This is responsible for downloading the precompiled NIFs for a given module.

  This task must only be used by Rustler's package creators who want to ship
  precompiled NIFs. The goal is to download precompiled packages and
  generate a checksum to check-in alongside the Hex repository. This is done
  by passing the `--all` flag.

  You can also use the `--only-local` flag to download only the precompiled
  package for use during development.

  This task also accept the `--print` flag to print the checksums.
  """

  use Mix.Task

  alias Rustler.Precompiled

  @impl true
  def run([module_name | maybe_flags]) do
    module = String.to_atom("Elixir.#{module_name}")

    urls =
      cond do
        "--all" in maybe_flags ->
          Precompiled.available_nif_urls(module)

        "--only-local" in maybe_flags ->
          [Precompiled.current_target_nif_url(module)]

        true ->
          raise "you need to specify either \"--all\" or \"--only-local\" flags"
      end

    result = Precompiled.download_nif_artifacts_with_checksums!(urls)

    if "--print" in maybe_flags do
      result
      |> Enum.map(fn map ->
        {Path.basename(Map.fetch!(map, :path)), Map.fetch!(map, :checksum)}
      end)
      |> Enum.sort()
      |> Enum.map_join("\n", fn {file, checksum} -> "#{checksum}  #{file}" end)
      |> IO.puts()
    end

    Precompiled.write_checksum!(module, result)
  end

  @impl true
  def run([]) do
    raise "the module name and a flag is expected. Use \"--all\" or \"--only-local\" flags"
  end
end
