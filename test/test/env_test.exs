defmodule RustlerTest.EnvTest do
  use ExUnit.Case, async: true

  test "owned environments" do
    shop_menu = [
      {:spaniel, 34},
      {:ring_of_power, 75},
      {:rope, 4}
    ]
    RustlerTest.sublists shop_menu
    receive do
      x ->
        assert x == [
          [],
          [{:rope, 4}],
          [{:ring_of_power, 75}],
          [{:ring_of_power, 75}, {:rope, 4}],
          [{:spaniel, 34}],
          [{:spaniel, 34}, {:rope, 4}],
          [{:spaniel, 34}, {:ring_of_power, 75}],
          [{:spaniel, 34}, {:ring_of_power, 75}, {:rope, 4}]
        ]
    end
  end
end
