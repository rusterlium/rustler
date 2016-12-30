defmodule RustlerTest.AtomTest do
  use ExUnit.Case, async: true

  test "atom to string conversion" do
    assert RustlerTest.atom_to_string(:hello) == "hello"
    assert RustlerTest.atom_to_string(:"hello world") == "hello world"
    assert RustlerTest.atom_to_string(2 + 2 == 4) == "true"
  end
end
