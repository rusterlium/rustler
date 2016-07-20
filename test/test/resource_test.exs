defmodule ResourceTest do
  use ExUnit.Case, async: true

  test "resource creation and interaction" do
    resource = TestNative.resource_make
    assert resource == "" # A resource looks like an empty binary :(
    assert TestNative.resource_get_integer_field(resource) == 0
    TestNative.resource_set_integer_field(resource, 10)
    assert TestNative.resource_get_integer_field(resource) == 10
  end
end
