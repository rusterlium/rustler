defmodule RustlerTest.CodegenTest do
  use ExUnit.Case, async: true

  test "tuple transcoder" do
    value = {1, 2}
    assert value == RustlerTest.tuple_echo(value)
  end

  test "map transcoder" do
    value = %{lhs: 1, rhs: 2}
    assert value == RustlerTest.map_echo(value)
  end

end
