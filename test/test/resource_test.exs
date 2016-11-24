defmodule RustlerTest.ResourceTest do
  use ExUnit.Case, async: true

  test "resource creation and interaction" do
    resource = RustlerTest.resource_make
    assert resource == "" # A resource looks like an empty binary :(
    assert RustlerTest.resource_get_integer_field(resource) == 0
    RustlerTest.resource_set_integer_field(resource, 10)
    assert RustlerTest.resource_get_integer_field(resource) == 10
  end
end
