defmodule RustlerTest.MapTest do
  use ExUnit.Case, async: true

  test "map iteration" do
    assert 3 == RustlerTest.sum_map_values(%{x: 1, y: 2})
    assert 0 == RustlerTest.sum_map_values(%{})
  end

  test "map iteration with keys" do
    entries = RustlerTest.map_entries(%{"d" => 0, "a" => 1, "b" => 7, "e" => 4, "c" => 6})

    assert [{"a", 1}, {"b", 7}, {"c", 6}, {"d", 0}, {"e", 4}] ==
             Enum.sort_by(entries, &elem(&1, 0))

    assert Enum.reverse(entries) ==
             RustlerTest.map_entries_reversed(%{"d" => 0, "a" => 1, "b" => 7, "e" => 4, "c" => 6})
  end

  test "map from arrays" do
    keys = Enum.into(1..10, [])
    values = Enum.into(11..20, [])
    expected_map = Enum.zip(keys, values) |> Enum.into(%{})
    assert expected_map == RustlerTest.map_from_arrays(keys, values)
  end

  test "map from arrays with non-matching length raises ArgumentError" do
    keys = Enum.into(1..10, [])
    values = []
    assert_raise(ArgumentError, fn -> RustlerTest.map_from_arrays(keys, values) end)
  end

  test "generic maps" do
    map = %{1 => "hello", 2 => "world"}
    assert map == RustlerTest.map_generic(map)

    assert_raise(ArgumentError, fn ->
      RustlerTest.map_generic(%{1 => "hello", not_a_number: "world"})
    end)
  end

  test "map from pairs" do
    pairs = []

    assert %{} == RustlerTest.map_from_pairs(pairs)

    pairs = [{"a", 1}, {"b", 7}, {"c", 6}, {"d", 0}, {"e", 4.1}, {"foo", "bar"}]

    assert %{"d" => 0, "a" => 1, "b" => 7, "e" => 4.1, "c" => 6, "foo" => "bar"} ==
             RustlerTest.map_from_pairs(pairs)

    pairs = [foo: [complex: true], bar: %{"simple" => false}]

    assert %{bar: %{"simple" => false}, foo: [complex: true]} ==
             RustlerTest.map_from_pairs(pairs)
  end

  test "map from pairs with incorrect type" do
    pairs = [{"a", 1}, {"b", 7}, {"c", 6}, {"d", 0}, nil]

    assert_raise(ArgumentError, fn ->
      RustlerTest.map_from_pairs(pairs)
    end)

    pairs = [false, true]

    assert_raise(ArgumentError, fn ->
      RustlerTest.map_from_pairs(pairs)
    end)
  end
end
