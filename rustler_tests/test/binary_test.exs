defmodule RustlerTest.BinaryTest do
  use ExUnit.Case, async: true

  test "subbinary creation" do
    assert RustlerTest.make_shorter_subbinary("test") == "es"
    assert_raise ArgumentError, fn -> RustlerTest.make_shorter_subbinary("t") end
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

  test "new binary creation" do
    assert RustlerTest.new_binary_new() == <<1, 2, 3, 4>>
  end

  test "unowned binary to owned" do
    assert RustlerTest.unowned_to_owned("test") == <<1, "est">>
    assert RustlerTest.unowned_to_owned("whatisgoingon") == <<1, "hatisgoingon">>
    # Notice that this also panic, but we are suppressing the message
    # at the function implementation.
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
    assert RustlerTest.decode_iolist(["hi", " ", "there"]) == "hi there"
    assert RustlerTest.decode_iolist([[?h, ?i], ~c" ", ["there"]]) == "hi there"
  end

  test "subbinary as term returns correct slice" do
    assert RustlerTest.subbinary_as_term("hello world", 6, 5) == "world"
    assert RustlerTest.subbinary_as_term("hello world", 0, 5) == "hello"
    assert RustlerTest.subbinary_as_term("hello", 0, 0) == ""
    assert RustlerTest.subbinary_as_term("hello", 0, 5) == "hello"
  end

  test "subbinary as term rejects out of bounds" do
    assert_raise ArgumentError, fn -> RustlerTest.subbinary_as_term("hello", 3, 5) end
    assert_raise ArgumentError, fn -> RustlerTest.subbinary_as_term("hello", 6, 0) end
  end

  test "subbinary as term matches make_subbinary" do
    input = "abcdefghij"
    # Both methods should produce identical binary content
    assert RustlerTest.subbinary_as_term(input, 1, 8) == RustlerTest.make_shorter_subbinary(input)
  end

  test "iolist decode returns binary" do
    assert RustlerTest.first_four_bytes_of_iolist("hi there") == "hi t"
    assert RustlerTest.first_four_bytes_of_iolist(["hi there"]) == "hi t"
    assert RustlerTest.first_four_bytes_of_iolist(["hi", " ", "there"]) == "hi t"
    assert RustlerTest.first_four_bytes_of_iolist([[?h, ?i], ~c" ", ["there"]]) == "hi t"
  end
end
