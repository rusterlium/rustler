defmodule RustlerTest.ListTest do
  use ExUnit.Case, async: true

  test "list iteration" do
    assert 8 == RustlerTest.sum_list([1, 2, 1, 4])
  end

  test "list iteration fails on improper lists" do
    assert_raise ErlangError, fn -> RustlerTest.sum_list([1, 4, 2 | :invalid]) end
  end

  test "list iteration fails on invalid entries" do
    assert_raise ArgumentError, fn -> RustlerTest.sum_list([1, 4, :invalid, 2]) end
  end

  test "simple list construction" do
    assert RustlerTest.make_list() == [1, 2, 3]
  end

  test "simple list construction with sum" do
    assert RustlerTest.sum_list(RustlerTest.make_list()) == 6
  end
end
