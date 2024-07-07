defmodule RustlerTest.ResourceTest do
  use ExUnit.Case, async: true
  import Bitwise

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

  test "resource binaries" do
    res = RustlerTest.resource_make_with_binaries()

    {slice, vec, static} = RustlerTest.resource_make_binaries(res)

    assert slice == vec
    assert vec == static
  end

  test "monitor resource" do
    resource = RustlerTest.monitor_resource_make()
    parent = self()

    spawn(fn ->
      RustlerTest.monitor_resource_monitor(resource, self())
      send(parent, :done)
    end)

    receive do
      :done -> :ok
    end

    :timer.sleep(10)
    assert RustlerTest.monitor_resource_down_called(resource) == true
  end

  test "monitor resource demonitor" do
    resource = RustlerTest.monitor_resource_make()
    parent = self()

    spawn(fn ->
      RustlerTest.monitor_resource_monitor(resource, self())
      RustlerTest.monitor_resource_demonitor(resource)
      send(parent, :done)
    end)

    receive do
      :done -> :ok
    end

    :timer.sleep(10)
    assert RustlerTest.monitor_resource_down_called(resource) == false
  end
end
