defmodule RustlerBenchmarksTest do
  use ExUnit.Case
  doctest RustlerBenchmarks

  test "greets the world" do
    assert RustlerBenchmarks.hello() == :world
  end
end
