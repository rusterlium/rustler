defmodule RustlerTest.AtomTest do
  use ExUnit.Case, async: true

  test "atom to string" do
    assert RustlerTest.atom_to_string(:test_atom) == "test_atom"
    assert RustlerTest.atom_to_string(true) == "true"
  end

  test "atom to string for non-unicode should panic" do
    assert catch_error(RustlerTest.atom_to_string(:erlang.list_to_atom([197]))) == :badarg
  end
end
