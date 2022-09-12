defmodule Benchmark.NifRecord do
  @moduledoc """
  Benchmark the performance of decoding/encoding records.
  """

  defmodule MyRecord do
    import Record

    defrecord(
      :my_record,
      ?a..?z
      |> Enum.to_list()
      |> Enum.map(fn c -> {[c] |> List.to_string() |> String.to_atom(), c} end)
    )
  end

  def run do
    require MyRecord

    input = MyRecord.my_record()

    # Sanity checks
    nil = Benchmark.nifrecord_benchmark(input, :decode)
    ^input = Benchmark.nifrecord_benchmark(input, :decode_and_encode)

    # Benchmark
    Benchee.run(%{
      "decode" => fn ->
        Benchmark.nifrecord_benchmark(input, :decode)
      end,
      "decode and encode" => fn ->
        Benchmark.nifrecord_benchmark(input, :decode_and_encode)
      end
    })
  end
end
