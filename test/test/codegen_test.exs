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

  test "unit enum transcoder" do
    assert :foo == RustlerTest.unit_enum_echo(:foo)
    assert :bar == RustlerTest.unit_enum_echo(:bar)
    assert :baz == RustlerTest.unit_enum_echo(:baz)
    assert :invalid_variant == RustlerTest.unit_enum_echo(:somethingelse)
  end

  test "untagged enum transcoder" do
    assert 123 == RustlerTest.untagged_enum_echo(123)
    assert "Hello" == RustlerTest.untagged_enum_echo("Hello")
    assert %AddStruct{lhs: 45, rhs: 123} = RustlerTest.untagged_enum_echo(%AddStruct{lhs: 45, rhs: 123})
    assert :invalid_variant == RustlerTest.untagged_enum_echo([1,2,3,4])
  end
end
