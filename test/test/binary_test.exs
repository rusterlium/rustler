defmodule RustlerTest.BinaryTest do
  use ExUnit.Case, async: true

  test "subbinary creation" do
    assert RustlerTest.make_shorter_subbinary("test") == "es"
  end

  test "unvalid subbinary creation" do
    assert_raise ArgumentError, fn -> RustlerTest.make_shorter_subbinary("t") end
  end
end
