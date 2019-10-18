defmodule RustlerTest.AtomTest do
  use ExUnit.Case, async: true

  test "atom to string" do
    assert RustlerTest.atom_to_string(:test_atom) == "test_atom"
    assert RustlerTest.atom_to_string(true) == "true"
    assert RustlerTest.atom_to_string(:erlang.list_to_atom([197])) == "Å"
  end

  test "binary to atom" do
    assert RustlerTest.binary_to_atom("test_atom") == :test_atom
  end

  test "binary to existing atom" do
    assert RustlerTest.binary_to_existing_atom("test_atom_nonexisting") == nil
    assert RustlerTest.binary_to_atom("test_atom_nonexisting")
    assert RustlerTest.binary_to_existing_atom("test_atom_nonexisting") != nil
  end

  test "atom to string for non-atom should raise" do
    assert catch_error(RustlerTest.atom_to_string("already a string")) == :badarg
  end

  test "atom equals ok" do
    assert RustlerTest.atom_equals_ok(:ok)
    refute RustlerTest.atom_equals_ok(:fish)
    assert catch_error(RustlerTest.atom_equals_ok("ok")) == :badarg
  end
end
