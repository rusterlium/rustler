defmodule RustlerTest.PrimitivesTest do
  use ExUnit.Case, async: true

  test "number decoding and encoding" do
    assert 3 == RustlerTest.add_u32(1, 2)
    assert 3 == RustlerTest.add_i32(6, -3)
    assert -3 == RustlerTest.add_i32(3, -6)
    assert 3 == RustlerTest.echo_u8(3)
  end

  test "number decoding should fail on invalid terms" do
    assert_raise ArgumentError, fn -> RustlerTest.add_u32(-1, 1) end
    assert_raise ArgumentError, fn -> RustlerTest.add_u32("1", 1) end
    assert_raise ArgumentError, fn -> RustlerTest.add_i32(2_147_483_648, 1) end
  end

  test "option decoding and encoding" do
    assert 33.0 == RustlerTest.option_inc(32.0)
    assert nil == RustlerTest.option_inc(nil)
    assert_raise ArgumentError, fn -> RustlerTest.option_inc("hello") end
  end

  test "erlang option decoding and encoding" do
    assert 33.0 == RustlerTest.erlang_option_inc(32.0)
    assert :undefined == RustlerTest.erlang_option_inc(:undefined)
    assert_raise ArgumentError, fn -> RustlerTest.erlang_option_inc("hello") end
  end

  test "result decoding and encoding" do
    assert {:ok, 1} == RustlerTest.result_to_int({:ok, true})
    assert {:ok, 0} == RustlerTest.result_to_int({:ok, false})
    assert {:error, "watwat"} == RustlerTest.result_to_int({:error, "wat"})
    assert_raise ArgumentError, fn -> RustlerTest.result_to_int({:great, true}) end
  end

  test "i128 support" do
    import Bitwise

    i = 1 <<< 62
    assert i == RustlerTest.echo_i128(i)
    assert -i == RustlerTest.echo_i128(-i)

    i = 1 <<< 126
    assert i == RustlerTest.echo_i128(i)
    assert -i == RustlerTest.echo_i128(-i)

    assert_raise ArgumentError, fn -> RustlerTest.echo_i128(:non_int) end
    assert_raise ArgumentError, fn -> RustlerTest.echo_i128(123.45) end
    assert_raise ArgumentError, fn -> RustlerTest.echo_i128(1 <<< 127) end
    assert_raise ArgumentError, fn -> RustlerTest.echo_i128(1 <<< 128) end
  end

  test "u128 support" do
    import Bitwise

    i = 1 <<< 63
    assert i == RustlerTest.echo_u128(i)

    i = 1 <<< 127
    assert i == RustlerTest.echo_u128(i)

    assert_raise ArgumentError, fn -> RustlerTest.echo_u128(:non_int) end
    assert_raise ArgumentError, fn -> RustlerTest.echo_u128(123.45) end
    assert_raise ArgumentError, fn -> RustlerTest.echo_i128(1 <<< 128) end
  end
end
