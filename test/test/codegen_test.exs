defmodule AddStruct do
  defstruct lhs: 0, rhs: 0
end

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

  test "struct transcoder" do
    value = %AddStruct{lhs: 45, rhs: 123}
    assert value == RustlerTest.struct_echo(value)
    assert :invalid_struct == RustlerTest.struct_echo(DateTime.utc_now())
  end
end
