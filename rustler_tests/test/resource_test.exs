defmodule RustlerTest.ResourceTest do
  use ExUnit.Case, async: true
  use Bitwise

  test "resource creation and interaction" do
    resource = RustlerTest.resource_make()
    # A resource looks like an empty binary < OTP20
    assert resource == "" || is_reference(resource)
    assert RustlerTest.resource_get_integer_field(resource) == 0
    RustlerTest.resource_set_integer_field(resource, 10)
    assert RustlerTest.resource_get_integer_field(resource) == 10
  end

  test "resource cleanup" do
    # Create a bunch of unreferenced resources for the GC to cleanup.
    for i <- 0..1000 do
      resource = RustlerTest.resource_make()
      RustlerTest.resource_set_integer_field(resource, i)
    end

    # Clean them up. Don't crash.
    :erlang.garbage_collect()
  end

  test "resource cleanup 2" do
    # Create a bunch of unreferenced resources for the GC to cleanup.
    for i <- 0..1000 do
      RustlerTest.resource_make_immutable(i * 0x11235813 &&& 0xFFFFFFFF)
    end

    # Clean them up. Don't crash.
    :erlang.garbage_collect()
    :timer.sleep(100)

    # Erlang's exact GC should have cleaned all that up.
    assert RustlerTest.resource_immutable_count() == 0
  end
end
