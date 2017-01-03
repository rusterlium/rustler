defmodule RustlerTest.AtomTest do
  use ExUnit.Case, async: true

  test "atom to string" do
    assert RustlerTest.atom_to_string(:test_atom) == "test_atom"
    assert RustlerTest.atom_to_string(true) == "true"
    assert RustlerTest.atom_to_string(:erlang.list_to_atom([197])) == "Å"
  end

  test "atom to string for non-atom should raise" do
    assert catch_error(RustlerTest.atom_to_string("already a string")) == :badarg
  end
end
