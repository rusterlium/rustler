defmodule AddStruct do
  defstruct lhs: 0, rhs: 0
end

defmodule AddRecord do
  import Record
  defrecord :record, [lhs: 1, rhs: 2]
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

  test "record transcoder" do
    import AddRecord
    value = record()
    assert value == RustlerTest.record_echo(value)
    assert :invalid_record == RustlerTest.record_echo({})
    assert :invalid_record == RustlerTest.record_echo({:wrong_tag, 1, 2})
    assert_raise ArgumentError, fn -> RustlerTest.record_echo(:somethingelse) end
  end

  test "unit enum transcoder" do
    assert :foo_bar == RustlerTest.unit_enum_echo(:foo_bar)
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
