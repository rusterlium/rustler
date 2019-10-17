defmodule RustlerTest.DirtyTest do
  use ExUnit.Case, async: true

  test "dirty io" do
    RustlerTest.dirty_io()
  end

  test "dirty cpu" do
    RustlerTest.dirty_cpu()
  end
end
