defmodule Benchmark.NifStruct do
  @moduledoc """
  Benchmark the performance of decoding/encoding structs.
  """

  defmodule MyStruct do
    defstruct ?a..?z
              |> Enum.to_list()
              |> Enum.map(fn c -> {[c] |> List.to_string() |> String.to_atom(), c} end)
  end

  def run do
    input = %MyStruct{}

    # Sanity checks
    nil = Benchmark.nifstruct_benchmark(input, :decode)
    ^input = Benchmark.nifstruct_benchmark(input, :decode_and_encode)

    # Benchmark
    Benchee.run(%{
      "decode" => fn ->
        Benchmark.nifstruct_benchmark(input, :decode)
      end,
      "decode and encode" => fn ->
        Benchmark.nifstruct_benchmark(input, :decode_and_encode)
      end
    })
  end
end
