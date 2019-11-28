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

  test "binary creation" do
    assert RustlerTest.binary_new() == <<1, 2, 3, 4>>
  end

  test "owned binary creation" do
    assert RustlerTest.owned_binary_new() == <<1, 2, 3, 4>>
  end

  test "unowned binary to owned" do
    assert RustlerTest.unowned_to_owned("test") == <<1, "est">>
    assert RustlerTest.unowned_to_owned("whatisgoingon") == <<1, "hatisgoingon">>
    assert_raise ErlangError, fn -> RustlerTest.unowned_to_owned("") end
  end

  test "realloc shrink binary" do
    assert RustlerTest.realloc_shrink() == <<1, 2, 3, 4>>
  end

  test "realloc grow binary" do
    assert RustlerTest.realloc_grow() == <<1, 2, 3, 4, 5>>
  end

  test "encode string" do
    assert RustlerTest.encode_string() == {"first", "second"}
  end

  test "decode iolist as binary" do
    assert RustlerTest.decode_iolist(["hi", " ", "there"]) == ["hi", " ", "there"]
  end
end
