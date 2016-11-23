case System.cmd("cargo", ["build"]) do
  {_, 0} -> nil
  {msg, _} -> exit(msg)
end

defmodule RustlerNative do
  def load do
    require Rustler
    :ok = Rustler.load_nif(:rustler_test, "rustler_test")
  end

  def add_u32(_lhs, _rhs), do: throw(:no_nif)
  def add_i32(_lhs, _rhs), do: throw(:no_nif)
  def tuple_add({_lhs, _rhs}), do: throw(:no_nif)
end
RustlerNative.load

ExUnit.start

defmodule RustlerBasicTest do
  use ExUnit.Case, async: true

  test "basic addition" do
    assert RustlerNative.add_u32(1, 1) == 2
    assert RustlerNative.add_u32(1923, 9928) == (1923 + 9928)
    assert RustlerNative.add_i32(-1, 1) == 0
  end

  test "invalid addition argument" do
    assert_raise ArgumentError, fn ->
      RustlerNative.add_u32(:cheese, 1)
    end
    assert_raise ArgumentError, fn ->
      RustlerNative.add_u32(1.2, 1)
    end
    assert_raise ArgumentError, fn ->
      RustlerNative.add_u32(-1, 1)
    end
  end

  test "tuple decode" do
    assert RustlerNative.tuple_add({21, 4}) == 25
    assert RustlerNative.tuple_add({21, -1}) == 20
  end
end
