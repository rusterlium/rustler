defmodule RustlerTest.BinaryTest do
  use ExUnit.Case, async: true

  test "subbinary creation" do
    assert RustlerTest.make_shorter_subbinary("test") == "es"
  end

  test "invalid subbinary creation" do
    assert_raise ErlangError, fn -> RustlerTest.make_shorter_subbinary("t") end
  end
end
