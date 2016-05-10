defmodule ListTest do
  use ExUnit.Case, async: true

  test "list iteration" do
    assert 8 == TestNative.sum_list([1, 2, 1, 4])
  end

  test "list iteration fails on improper lists" do
    assert_raise ErlangError, fn -> TestNative.sum_list([1, 4, 2 | :invalid]) end
  end

  test "list iteration fails on invalid entries" do
    assert_raise ArgumentError, fn -> TestNative.sum_list([1, 4, :invalid, 2]) end
  end
end
