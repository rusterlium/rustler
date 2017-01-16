defmodule RustlerTest.BinaryTest do
  use ExUnit.Case, async: true

  test "subbinary creation" do
    assert RustlerTest.make_shorter_subbinary("test") == "es"
    assert_raise ErlangError, fn -> RustlerTest.make_shorter_subbinary("t") end
  end

  test "parse integer from binary" do
    assert RustlerTest.parse_integer("12") == 12
    assert RustlerTest.parse_integer("-254") == -254
    assert_raise ArgumentError, fn -> RustlerTest.parse_integer("test") end
    assert_raise ArgumentError, fn -> RustlerTest.parse_integer("999999999999999999999") end
  end

end
