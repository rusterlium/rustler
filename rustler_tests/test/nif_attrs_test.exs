defmodule NifAttrsTest do
  use ExUnit.Case

  test "can rename a NIF with an attribute" do
    assert RustlerTest.nif_attrs_can_rename()
  end
end
