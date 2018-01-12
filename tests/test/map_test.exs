defmodule RustlerTest.MapTest do
  use ExUnit.Case, async: true

  test "map iteration" do
    assert 3 == RustlerTest.sum_map_values(%{x: 1, y: 2})
    assert 0 == RustlerTest.sum_map_values(%{})
  end

  test "map iteration with keys" do
    assert [{"a", 1}, {"b", 7}, {"c", 6}, {"d", 0}, {"e", 4}] ==
      RustlerTest.map_entries_sorted(%{"d" => 0, "a" => 1, "b" => 7, "e" => 4, "c" => 6})
  end
end
