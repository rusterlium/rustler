defmodule SerdeRustlerTests.NifTest.NewtypeStruct do
  @moduledoc "`struct NewtypeStruct(u8)`"
  require Record
  @type t :: {__MODULE__, non_neg_integer}
  Record.defrecord(:record, __MODULE__, num: 0)
end

defmodule SerdeRustlerTests.NifTest.NewtypeVariant do
  defmodule N do
    @moduledoc "`enum NewtypeVariant { N(u8) }`"
    require Record
    @type t :: {__MODULE__, non_neg_integer}
    Record.defrecord(:record, __MODULE__, num: 0)
  end
end

defmodule SerdeRustlerTests.NifTest.TupleStruct do
  @moduledoc "`struct TupleStruct(u8, u8, u8)`"
  require Record
  @type t :: {__MODULE__, non_neg_integer, non_neg_integer, non_neg_integer}
  Record.defrecord(:record, __MODULE__, r: 0, g: 0, b: 0)
end

defmodule SerdeRustlerTests.NifTest.TupleVariant do
  defmodule T do
    @moduledoc "`enum TupleVariant { T(u8, u8) }`"
    require Record
    @type t :: {__MODULE__, non_neg_integer, non_neg_integer}
    Record.defrecord(:record, __MODULE__, a: 0, b: 0)
  end
end

defmodule SerdeRustlerTests.NifTest.Struct do
  @moduledoc "`struct Struct {...}`"
  defstruct r: 0, g: 0, b: 0
end

defmodule SerdeRustlerTests.NifTest.StructVariant do
  defmodule S do
    @moduledoc "`enum StructVariant { S{...} }`"
    defstruct r: 0, g: 0, b: 0
  end
end

defmodule SerdeRustlerTests.NifTest do
  @moduledoc """
  Tests the Serializer and Deserializer trait's behaviour against the primitives, records and structs defined here and the enums and structs defined in `native/serde_rustler_tests/src/types.rs`.
  """

  use ExUnit.Case

  alias SerdeRustlerTests.Helpers

  alias SerdeRustlerTests.NifTest.{
    NewtypeStruct,
    NewtypeVariant,
    TupleStruct,
    TupleVariant,
    Struct,
    StructVariant
  }

  require NewtypeStruct
  require NewtypeVariant.N
  require TupleStruct
  require TupleVariant.T

  defp run_tests(test_name, expected_term, ctx) do
    Helpers.run_ser(test_name, expected_term)
    Helpers.run_de(test_name, expected_term)

    unless ctx[:skip] == :transcode do
      Helpers.run_transcode(test_name, expected_term)
    end
  end

  describe "Primitive Types:" do
    test "option", ctx do
      run_tests("none", nil, ctx)
      run_tests("some", 100, ctx)
    end

    test "boolean", ctx do
      run_tests("true", true, ctx)
      run_tests("false", false, ctx)
    end

    test "i8", ctx do
      run_tests("i8 (min)", -128, ctx)
      run_tests("i8 (0)", 0, ctx)
      run_tests("i8 (max)", 127, ctx)
    end

    test "i16", ctx do
      run_tests("i16 (min)", -32_768, ctx)
      run_tests("i16 (0)", 0, ctx)
      run_tests("i16 (max)", 32_767, ctx)
    end

    test "i32", ctx do
      run_tests("i32 (min)", -2_147_483_648, ctx)
      run_tests("i32 (0)", 0, ctx)
      run_tests("i32 (max)", 2_147_483_647, ctx)
    end

    test "i64", ctx do
      run_tests("i64 (min)", -9_223_372_036_854_775_808, ctx)
      run_tests("i64 (0)", 0, ctx)
      run_tests("i64 (max)", 9_223_372_036_854_775_807, ctx)
    end

    @tag :skip
    test "i128", ctx do
      run_tests("i128 (min)", 0, ctx)
      run_tests("128 (0)", 0, ctx)
      run_tests("i128 (max)", 0, ctx)
    end

    test "u8", ctx do
      run_tests("u8 (min)", 0, ctx)
      run_tests("u8 (max)", 255, ctx)
    end

    test "u16", ctx do
      run_tests("u16 (min)", 0, ctx)
      run_tests("u16 (max)", 65_535, ctx)
    end

    test "u32", ctx do
      run_tests("u32 (min)", 0, ctx)
      run_tests("u32 (max)", 4_294_967_295, ctx)
    end

    test "u64", ctx do
      run_tests("u64 (min)", 0, ctx)
      run_tests("u64 (max)", 18_446_744_073_709_551_615, ctx)
    end

    @tag :skip
    test "u128", ctx do
      run_tests("u128 (min)", 0, ctx)
      run_tests("u128 (max)", 0, ctx)
    end

    test "f32", ctx do
      test_case = Helpers.to_float(<<0x00, 0x00, 0x00, 0x00>>)
      run_tests("f32 (0)", test_case, ctx)

      test_case = Helpers.to_float(<<0x80, 0x00, 0x00, 0x00>>)
      run_tests("f32 (-0)", test_case, ctx)

      test_case = Helpers.to_float(<<0x3F, 0x80, 0x00, 0x00>>)
      run_tests("f32 (one)", test_case, ctx)

      test_case = Helpers.to_float(<<0x00, 0x00, 0x00, 0x01>>)
      run_tests("f32 (smallest subnormal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x00, 0x7F, 0xFF, 0xFF>>)
      run_tests("f32 (largest subnormal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x00, 0x80, 0x00, 0x00>>)
      run_tests("f32 (smallest normal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x7F, 0x7F, 0xFF, 0xFF>>)
      run_tests("f32 (largest normal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x3F, 0x80, 0x00, 0x01>>)
      run_tests("f32 (smallest number < 1)", test_case, ctx)

      test_case = Helpers.to_float(<<0x3F, 0x7F, 0xFF, 0xFF>>)
      run_tests("f32 (largest number < 1)", test_case, ctx)

      # run_tests("f32 (infinity)", Helpers.to_float(<<0x7f, 0x80, 0x00, 0x00>>), ctx)
      # run_tests("f32 (-infinity)", Helpers.to_float(<<0xff, 0x80, 0x00, 0x00>>), ctx)
    end

    test "f64", ctx do
      test_case = Helpers.to_float(<<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>)
      run_tests("f64 (0)", test_case, ctx)

      test_case = Helpers.to_float(<<0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>)
      run_tests("f64 (-0)", test_case, ctx)

      test_case = Helpers.to_float(<<0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>)
      run_tests("f64 (one)", test_case, ctx)

      test_case = Helpers.to_float(<<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01>>)
      run_tests("f64 (smallest subnormal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x00, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>)
      run_tests("f64 (largest subnormal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>)
      run_tests("f64 (smallest normal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x7F, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>)
      run_tests("f64 (largest normal)", test_case, ctx)

      test_case = Helpers.to_float(<<0x3F, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01>>)
      run_tests("f64 (smallest number < 1)", test_case, ctx)

      test_case = Helpers.to_float(<<0x3F, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>)
      run_tests("f64 (largest number < 1)", test_case, ctx)

      # run_tests("f64 (infinity)", Helpers.to_float(<<0x7f, 0x80, 0x00, 0x00, 0x00, 0x00,, ctx 0x00, 0x00>>))
      # run_tests("f64 (-infinity)", Helpers.to_float(<<0xff, 0x80, 0x00, 0x00, 0x00, 0x00,, ctx 0x00, 0x00>>))
    end

    test "chars", ctx do
      run_tests("char (ascii)", ~c"A", ctx)
      run_tests("char (replacement)", ~c"�", ctx)
    end

    test "strings", ctx do
      run_tests("str (empty)", "", ctx)
      run_tests("str", "hello world", ctx)
    end

    test "bytes", ctx do
      run_tests("bytes", <<3, 2, 1, 0>>, ctx)
    end
  end

  describe "Unit Types:" do
    test "unit", ctx do
      run_tests("unit", nil, ctx)
    end

    test "unit struct", ctx do
      run_tests("unit struct", nil, ctx)
    end

    test "unit variant", ctx do
      test_case = :"UnitVariant::A"
      transcoded = "UnitVariant::A"
      run_tests("unit variant", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("unit variant", test_case, transcoded)
    end
  end

  describe "Newtype Types:" do
    test "newtype struct", ctx do
      test_case = NewtypeStruct.record(num: 255)
      transcoded = [~s"#{NewtypeStruct}", 255]
      run_tests("newtype struct", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("newtype struct", test_case, transcoded)
    end

    test "newtype variant", ctx do
      test_case = NewtypeVariant.N.record(num: 255)
      transcoded = [~s"#{NewtypeVariant.N}", 255]
      run_tests("newtype variant", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("newtype struct", test_case, transcoded)
    end

    test "newtype variant (Result::Ok(T), or {:ok, T})", ctx do
      test_case = {:ok, 255}
      transcoded = ["Ok", 255]
      run_tests("newtype variant (ok tuple)", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("newtype variant (ok tuple)", test_case, transcoded)
    end

    test "newtype variant (Result::Err(T), or {:error, T}", ctx do
      test_case = {:error, "error reason"}
      transcoded = ["Err", "error reason"]
      run_tests("newtype variant (error tuple)", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("newtype variant (error tuple)", test_case, transcoded)
    end
  end

  describe "Sequences:" do
    test "sequences (empty)", ctx do
      run_tests("sequences (empty)", [], ctx)
    end

    test "sequences (primitive)", ctx do
      run_tests("sequences (primitive)", ["hello", "world"], ctx)
    end

    test "sequences (complex)", ctx do
      test_case = [NewtypeStruct.record(num: 0), NewtypeStruct.record(num: 255)]
      transcoded = [[~s"#{NewtypeStruct}", 0], [~s"#{NewtypeStruct}", 255]]
      run_tests("sequences (complex)", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("sequences (complex)", test_case, transcoded)
    end
  end

  describe "Tuple Types:" do
    test "tuple (empty)", ctx do
      run_tests("tuple (empty)", nil, ctx)
    end

    test "tuple", ctx do
      test_case = {0, 255}
      transcoded = [0, 255]
      run_tests("tuple", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("tuple", test_case, transcoded)
    end

    test "tuple struct", ctx do
      test_case = TupleStruct.record(r: 0, g: 128, b: 255)
      transcoded = [~s"#{TupleStruct}", 0, 128, 255]
      run_tests("tuple struct", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("tuple struct", test_case, transcoded)
    end

    test "tuple variant", ctx do
      test_case = TupleVariant.T.record(a: 0, b: 255)
      transcoded = [~s"#{TupleVariant.T}", 0, 255]
      run_tests("tuple variant", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("tuple variant", test_case, transcoded)
    end
  end

  describe "Map and Struct Types:" do
    test "map (primitive)", ctx do
      simple_map = %{"key" => "hello", "val" => "world"}
      run_tests("map (primitive)", simple_map, ctx)
    end

    test "map (complex)", ctx do
      test_case = %{
        "key" => %Struct{r: 0, g: 0, b: 0},
        "val" => %Struct{r: 255, g: 255, b: 255}
      }

      transcoded = %{
        "key" => %{"r" => 0, "g" => 0, "b" => 0},
        "val" => %{"r" => 255, "g" => 255, "b" => 255}
      }

      run_tests("map (complex)", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("map (complex)", test_case, transcoded)
    end

    test "struct", ctx do
      test_case = %Struct{r: 0, g: 128, b: 255}
      transcoded = %{"r" => 0, "g" => 128, "b" => 255}
      run_tests("struct", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("struct", test_case, transcoded)
    end

    test "struct variant", ctx do
      test_case = %StructVariant.S{r: 0, g: 128, b: 255}
      transcoded = %{"r" => 0, "g" => 128, "b" => 255}
      run_tests("struct variant", test_case, Helpers.skip(ctx, :transcode))
      Helpers.run_transcode("struct variant", test_case, transcoded)
    end
  end
end
