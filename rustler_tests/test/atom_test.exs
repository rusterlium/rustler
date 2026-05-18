defmodule RustlerTest.AtomTest do
  use ExUnit.Case, async: true

  test "atom to string" do
    assert RustlerTest.atom_to_string(:test_atom) == "test_atom"
    assert RustlerTest.atom_to_string(true) == "true"
    assert RustlerTest.atom_to_string(:erlang.list_to_atom([197])) == "Å"
  end

  test "utf8 atom roundtrip" do
    utf8 = "ÀrgerÖ"

    utf8_atom = RustlerTest.binary_to_atom_utf8(utf8)
    assert utf8_atom == String.to_atom(utf8)
    assert RustlerTest.atom_to_string(utf8_atom) == utf8
  end

  test "utf8 atom roundtrip on nif 2.17+" do
    if RustlerTest.Helper.has_nif_version("2.17") do
      utf8 = "こんにちは"

      utf8_atom = RustlerTest.binary_to_atom_utf8(utf8)
      assert utf8_atom == String.to_atom(utf8)
      assert RustlerTest.atom_to_string(utf8_atom) == utf8
    end
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

  test "binary to existing atom utf8" do
    utf8 = "test_non_existing_ÀrgerÖ"

    assert RustlerTest.binary_to_existing_atom_utf8(utf8) == nil
    assert RustlerTest.binary_to_atom_utf8(utf8)
    assert RustlerTest.binary_to_existing_atom_utf8(utf8) != nil

    if RustlerTest.Helper.has_nif_version("2.17") do
      utf8 = "test_non_existing_こんにちは"

      assert RustlerTest.binary_to_existing_atom_utf8(utf8) == nil
      assert RustlerTest.binary_to_atom_utf8(utf8)
      assert RustlerTest.binary_to_existing_atom_utf8(utf8) != nil
    end
  end

  test "reject invalid utf8" do
    invalid_utf8 = <<0xE5>>

    assert catch_error(RustlerTest.binary_to_atom_utf8(invalid_utf8)) == :badarg
    assert RustlerTest.binary_to_existing_atom_utf8(invalid_utf8) == nil
  end

  test "reject not-latin1-encodable string" do
    unless RustlerTest.Helper.has_nif_version("2.17") do
      non_latin1_utf8 = "こんにちは"

      assert catch_error(RustlerTest.binary_to_atom_utf8(non_latin1_utf8)) == :badarg
      assert RustlerTest.binary_to_existing_atom_utf8(non_latin1_utf8) == nil
    end
  end
end
