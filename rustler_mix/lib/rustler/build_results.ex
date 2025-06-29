defmodule Rustler.BuildResults do
  @moduledoc """
  Struct for holding a list of build artifacts produced by Rustler.
  """
  defstruct artifacts: []
end

defmodule Rustler.BuildArtifact do
  @moduledoc """
  Represents a single build artifact produced by the Rust build process.

  Fields:
    * `:kind` - The type of artifact (`:lib` or `:bin`)
    * `:name` - The name of the artifact
    * `:filename` - The path to the artifact file
  """
  defstruct [:kind, :name, :filename]

  def from_line(line) do
    if line["reason"] == "compiler-artifact" do
      target = line["target"]

      base = %__MODULE__{
        name: target["name"],
        filename: line["filenames"] |> :erlang.hd()
      }

      kind = target["kind"]

      maybe_lib =
        if kind |> Enum.member?("dylib") || kind |> Enum.member?("cdylib") do
          [%{base | kind: :lib}]
        end || []

      maybe_exec =
        if kind |> Enum.member?("bin") do
          [%{base | kind: :bin, filename: line["executable"]}]
        end || []

      maybe_lib ++ maybe_exec
    end || []
  end
end

defimpl Collectable, for: Rustler.BuildResults do
  def into(results) do
    maybe_log_msg = fn elem ->
      if elem["reason"] == "compiler-message" do
        # Log message:
        # Mix.shell().info(elem["message"]["message"])
      end
    end

    collector_fun = fn
      acc, {:cont, elem} ->
        case Jason.decode(elem) do
          {:ok, elem} ->
            maybe_log_msg.(elem)
            update_in(acc.artifacts, &(&1 ++ Rustler.BuildArtifact.from_line(elem)))

          {:error, _err} ->
            acc
        end

      acc, :done ->
        acc

      _acc, :halt ->
        :ok
    end

    {results, collector_fun}
  end
end
