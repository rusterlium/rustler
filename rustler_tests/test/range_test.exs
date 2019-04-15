defmodule RustlerTest.RangeTest do
  use ExUnit.Case, async: true

  test "range iteration" do
    assert 55 == RustlerTest.sum_range(1..10)
  end
end
