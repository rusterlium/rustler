defmodule Benchmark.NifVarious do
  @moduledoc """
  Benchmark the performance of decoding/encoding enums & others.
  """

  defmodule TestStruct do
    defstruct [:a, :b, :c, :d]
  end

  defmodule TestStructString do
    defstruct [:a]
  end

  def run do
    test_struct = %TestStruct{
      a: "abcd",
      b: 6_000_000_000,
      c: nil,
      d: true
    }

    test_struct_string = %TestStructString{
      a: "abcd"
    }

    test_struct_enum = {:test_struct, %TestStruct{
      a: "abcd",
      b: 6_000_000_000,
      c: nil,
      d: true
    }}

    # Benchmark
    Benchee.run(%{
      "encode_atom" => fn -> Benchmark.encode_atom() end,
      "compare_atom" => fn -> Benchmark.compare_atom(:test) end,
      "void" => fn -> Benchmark.void() end,
      "decode_term" => fn -> Benchmark.decode_term("abcd") end,
      "struct_string_decode" => fn ->
        Benchmark.decode_struct_string(test_struct_string)
      end,
      "string_decode" => fn -> Benchmark.decode_string("abcd") end,
      "struct_decode" => fn -> Benchmark.decode_struct(test_struct) end,
      "tagged_enum_decode" => fn -> Benchmark.decode_tagged_enum(test_struct_enum) end,
      "tagged_enum_encode" => fn -> Benchmark.encode_tagged_enum() end
    })
  end
end
