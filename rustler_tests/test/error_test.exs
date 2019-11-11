defmodule RustlerTest.ErrorTest do
  use ExUnit.Case, async: true

  test "in elixir Error::BadArg results in an ArgumentError" do
    assert_raise(ArgumentError, fn -> RustlerTest.bad_arg_error() end)
  end

  test "in elixir Error::Atom results in a returned atom" do
    assert RustlerTest.atom_str_error() == :should_be_a_returned_atom
  end

  test "raise_atom_error raises an ErlangError with a raise" do
    exception = assert_raise(ErlangError, fn -> RustlerTest.raise_atom_error() end)
    assert exception == %ErlangError{original: :should_be_a_raised_atom}
  end

  test "raise_term_with_string_error returns the expected atom" do
    exception = assert_raise(ErlangError, fn -> RustlerTest.raise_term_with_string_error() end)
    assert exception == %ErlangError{original: "should_be_a_raised_string"}
  end

  test "raise_term_with_atom_error panics with a raise" do
    assert_raise(ErlangError, fn -> RustlerTest.raise_term_with_atom_error() end)
  end

  test "return_term_with_tuple_error returns an arbitrary Encoder" do
    assert RustlerTest.term_with_tuple_error() ==
             {:error, :should_be_an_atom_wrapped_in_an_error_tuple}
  end
end
