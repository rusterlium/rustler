defmodule AddStruct do
  defstruct lhs: 0, rhs: 0
end

defmodule GenericStruct do
  defstruct lhs: 0, rhs: "hi"
end

defmodule AddException do
  defexception message: ""
end

defmodule GenericException do
  defexception message: "", t: 0
end

defmodule AddRecord do
  import Record
  defrecord :record, lhs: 1, rhs: 2
end

defmodule GenericRecord do
  import Record
  defrecord :generic_record, field1: 1, field2: "hi"
end

defmodule NewtypeRecord do
  import Record
  defrecord :newtype, a: 1
end

defmodule TupleStructRecord do
  import Record
  defrecord :tuplestruct, a: 1, b: 2, c: 3
end

defmodule RustlerTest.CodegenTest do
  use ExUnit.Case, async: true

  describe "tuple" do
    test "transcoder" do
      value = {1, 2}
      assert value == RustlerTest.tuple_echo(value)
    end

    test "with invalid tuple" do
      value = {"invalid", 2}

      assert_raise ErlangError, "Erlang error: \"Could not decode field lhs on AddTuple\"", fn ->
        RustlerTest.tuple_echo(value)
      end
    end
  end

  describe "generic tuple" do
    test "transcoder" do
      value = {1, 2}
      assert value == RustlerTest.generic_tuple_echo_usize(value)

      value = {"foo", "bar"}
      assert value == RustlerTest.generic_tuple_echo_str(value)
    end

    test "with invalid tuple" do
      # This is invalid because they have different types
      value = {"invalid", 2}

      assert_raise ErlangError,
                   "Erlang error: \"Could not decode field t1 on GenericTuple\"",
                   fn ->
                     RustlerTest.generic_tuple_echo_usize(value)
                   end
    end
  end

  describe "generic tuple 2" do
    test "transcoder" do
      value = {1, -23, "foo"}
      assert value == RustlerTest.generic_tuple2_echo(value)
    end

    test "with invalid tuple" do
      # This is invalid because they have different types
      value = {1, "invalid", :bar}

      assert_raise ErlangError,
                   "Erlang error: \"Could not decode field u on GenericTuple2\"",
                   fn ->
                     RustlerTest.generic_tuple2_echo(value)
                   end
    end
  end

  describe "map" do
    test "transcoder" do
      value = %{lhs: 1, rhs: 2}
      assert value == RustlerTest.map_echo(value)
    end

    test "with invalid map" do
      value = %{lhs: "invalid", rhs: 2}

      assert_raise ErlangError, "Erlang error: \"Could not decode field :lhs on %{}\"", fn ->
        assert value == RustlerTest.map_echo(value)
      end
    end
  end

  describe "generic map" do
    test "transcoder" do
      value = %{lhs: 1, rhs: "hi"}
      assert value == RustlerTest.generic_map_echo(value)
    end

    test "with invalid map" do
      value = %{lhs: "invalid", rhs: 2}

      assert_raise ErlangError, "Erlang error: \"Could not decode field :lhs on %{}\"", fn ->
        assert value == RustlerTest.generic_map_echo(value)
      end
    end
  end

  describe "struct" do
    test "transcoder" do
      value = %AddStruct{lhs: 45, rhs: 123}
      assert value == RustlerTest.struct_echo(value)

      assert %ErlangError{original: :invalid_struct} ==
               assert_raise(ErlangError, fn ->
                 RustlerTest.struct_echo(DateTime.utc_now())
               end)
    end

    test "with invalid struct" do
      value = %AddStruct{lhs: "lhs", rhs: 123}

      assert_raise ErlangError,
                   "Erlang error: \"Could not decode field :lhs on %AddStruct{}\"",
                   fn ->
                     RustlerTest.struct_echo(value)
                   end
    end
  end

  describe "generic struct" do
    test "transcoder" do
      value = %GenericStruct{lhs: 45, rhs: "hi"}
      assert value == RustlerTest.generic_struct_echo(value)

      assert %ErlangError{original: :invalid_struct} ==
               assert_raise(ErlangError, fn ->
                 RustlerTest.generic_struct_echo(DateTime.utc_now())
               end)
    end

    test "with invalid struct" do
      value = %GenericStruct{lhs: "lhs", rhs: 123}

      assert_raise ErlangError,
                   "Erlang error: \"Could not decode field :lhs on %GenericStruct{}\"",
                   fn ->
                     RustlerTest.generic_struct_echo(value)
                   end
    end
  end

  describe "exception" do
    test "transcoder" do
      value = %AddException{message: "testing"}
      assert value == RustlerTest.exception_echo(value)

      assert %ErlangError{original: :invalid_struct} ==
               assert_raise(ErlangError, fn ->
                 RustlerTest.exception_echo(DateTime.utc_now())
               end)
    end

    test "with invalid struct" do
      value = %AddException{message: 'this is a charlist'}

      assert_raise ErlangError,
                   "Erlang error: \"Could not decode field :message on %AddException{}\"",
                   fn ->
                     RustlerTest.exception_echo(value)
                   end
    end
  end

  describe "generic exception" do
    test "transcoder" do
      value = %GenericException{message: "testing"}
      assert value == RustlerTest.generic_exception_echo(value)

      assert %ErlangError{original: :invalid_struct} ==
               assert_raise(ErlangError, fn ->
                 RustlerTest.generic_exception_echo(DateTime.utc_now())
               end)
    end

    test "with invalid struct" do
      value = %GenericException{message: 'this is a charlist', t: 23}

      assert_raise ErlangError,
                   "Erlang error: \"Could not decode field :message on %GenericException{}\"",
                   fn ->
                     RustlerTest.generic_exception_echo(value)
                   end
    end
  end

  describe "record" do
    test "transcoder" do
      require AddRecord
      value = AddRecord.record()
      assert value == RustlerTest.record_echo(value)

      assert %ErlangError{original: :invalid_record} ==
               assert_raise(ErlangError, fn -> RustlerTest.record_echo({}) end)

      assert %ErlangError{original: :invalid_record} ==
               assert_raise(ErlangError, fn ->
                 RustlerTest.record_echo({:wrong_tag, 1, 2})
               end)
    end

    test "with invalid Record structure" do
      assert_raise ErlangError, "Erlang error: \"Invalid Record structure for AddRecord\"", fn ->
        RustlerTest.record_echo(:somethingelse)
      end
    end

    test "with invalid Record" do
      require AddRecord
      value = AddRecord.record(lhs: 5, rhs: "invalid")
      message = "Erlang error: \"Could not decode field rhs on Record AddRecord\""

      assert_raise ErlangError, message, fn -> RustlerTest.record_echo(value) end
    end
  end

  describe "generic record" do
    test "transcoder" do
      require GenericRecord
      value = GenericRecord.generic_record()
      assert value == RustlerTest.generic_record_echo(value)

      assert %ErlangError{original: :invalid_record} ==
               assert_raise(ErlangError, fn -> RustlerTest.generic_record_echo({}) end)

      assert %ErlangError{original: :invalid_record} ==
               assert_raise(ErlangError, fn ->
                 RustlerTest.generic_record_echo({:wrong_tag, 1, "hi"})
               end)
    end

    test "with invalid Record structure" do
      assert_raise ErlangError,
                   "Erlang error: \"Invalid Record structure for GenericRecord\"",
                   fn ->
                     RustlerTest.generic_record_echo(:somethingelse)
                   end
    end

    test "with invalid Record" do
      require GenericRecord
      value = GenericRecord.generic_record(field1: :invalid, field2: 23)
      message = "Erlang error: \"Could not decode field field1 on Record GenericRecord\""

      assert_raise ErlangError, message, fn -> RustlerTest.generic_record_echo(value) end
    end
  end

  test "unit enum transcoder" do
    assert :foo_bar == RustlerTest.unit_enum_echo(:foo_bar)
    assert :baz == RustlerTest.unit_enum_echo(:baz)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn -> RustlerTest.unit_enum_echo(:somethingelse) end)
  end

  test "tagged enum transcoder 1" do
    assert {:named, %{x: 1, y: 2}} ==
             RustlerTest.tagged_enum_1_echo({:named, %{x: 1, y: 2}})

    assert {:named, %{x: 1, y: 2}} =
             RustlerTest.tagged_enum_1_echo({:named, %{x: 1, y: 2, extra: 3}})

    assert {:string1, "hello"} == RustlerTest.tagged_enum_1_echo({:string1, "hello"})
    assert {:string2, "world"} == RustlerTest.tagged_enum_1_echo({:string2, "world"})
    assert :untagged == RustlerTest.tagged_enum_1_echo(:untagged)
  end

  test "tagged enum transcoder 1 raising errors" do
    assert %ErlangError{original: "The second element of the tuple must be a map"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:named, "not a map"})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({"named", %{x: 1, y: 2}})
             end)

    assert %ErlangError{original: "Could not decode field 'x' on Enum 'TaggedEnum1'"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:named, %{x: "string", y: 2}})
             end)

    assert %ErlangError{original: "Could not decode field 'y' on Enum 'TaggedEnum1'"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:named, %{x: 1}})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:named})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo(nil)
             end)

    assert %ErlangError{original: "Could not decode field on position 1"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:string1, %{a: :map}})
             end)

    assert %ErlangError{original: "Could not decode field on position 1"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:string2, 10})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:untagged, :not_even_a_variant})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo({:not_exists, :not_even_a_variant})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_1_echo(:not_exists)
             end)
  end

  test "tagged enum transcoder 2" do
    assert :untagged == RustlerTest.tagged_enum_2_echo(:untagged)

    assert {:hash_map, %{1 => 1, 2 => 4}} ==
             RustlerTest.tagged_enum_2_echo({:hash_map, %{1 => 1, 2 => 4}})

    assert {:tuple, 1, 2} ==
             RustlerTest.tagged_enum_2_echo({:tuple, 1, 2})

    assert {:named, %{s: "Hello"}} == RustlerTest.tagged_enum_2_echo({:named, %{s: "Hello"}})
    assert {:enum, :untagged} == RustlerTest.tagged_enum_2_echo({:enum, :untagged})

    assert {:enum, {:string1, "world"}} ==
             RustlerTest.tagged_enum_2_echo({:enum, {:string1, "world"}})
  end

  test "tagged enum transcoder 2 raising errors" do
    assert %ErlangError{original: "Could not decode field on position 1"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_2_echo({:hash_map, %{a: "different", b: "type"}})
             end)

    assert %ErlangError{original: "The tuple must have 3 elements, but it has 4"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_2_echo({:tuple, 1, 2, 3})
             end)

    assert %ErlangError{original: "The tuple must have 3 elements, but it has 2"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_2_echo({:tuple, 1})
             end)

    assert %ErlangError{original: "The second element of the tuple must be a map"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_2_echo({:named, a: "not a map", b: "keywords"})
             end)

    assert %ErlangError{original: "Could not decode field on position 1"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_2_echo({:enum, {:foo, :too, :many, :elements}})
             end)
  end

  test "tagged enum transcoder 3" do
    assert {:struct, %AddStruct{lhs: 45, rhs: 123}} ==
             RustlerTest.tagged_enum_3_echo({:struct, %AddStruct{lhs: 45, rhs: 123}})

    assert {:named, %{lhs: 45, rhs: 123}} ==
             RustlerTest.tagged_enum_3_echo({:named, %{lhs: 45, rhs: 123}})

    assert %ErlangError{original: "Could not decode field on position 1"} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_3_echo({:struct, %{lhs: 45, rhs: 123}})
             end)

    assert {:named, %{lhs: 45, rhs: 123}} ==
             RustlerTest.tagged_enum_3_echo({:named, %AddStruct{lhs: 45, rhs: 123}})

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_3_echo({})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_3_echo(%{})
             end)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.tagged_enum_3_echo({nil})
             end)
  end

  test "untagged enum transcoder" do
    assert 123 == RustlerTest.untagged_enum_echo(123)
    assert "Hello" == RustlerTest.untagged_enum_echo("Hello")
    assert RustlerTest.untagged_enum_echo(true)

    assert %AddStruct{lhs: 45, rhs: 123} =
             RustlerTest.untagged_enum_echo(%AddStruct{lhs: 45, rhs: 123})

    assert true == RustlerTest.untagged_enum_echo(true)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.untagged_enum_echo([1, 2, 3, 4])
             end)
  end

  test "generic untagged enum transcoder" do
    assert 123 == RustlerTest.generic_untagged_enum_echo(123)
    assert %{lhs: 23, rhs: 42} == RustlerTest.generic_untagged_enum_echo(%{lhs: 23, rhs: 42})
    assert "hello" == RustlerTest.generic_untagged_enum_echo("hello")
    assert RustlerTest.generic_untagged_enum_echo(true)

    assert %ErlangError{original: :invalid_variant} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.generic_untagged_enum_echo([1, 2, 3, 4])
             end)
  end

  test "untagged enum with truthy" do
    assert %AddStruct{lhs: 45, rhs: 123} =
             RustlerTest.untagged_enum_with_truthy(%AddStruct{lhs: 45, rhs: 123})

    assert true == RustlerTest.untagged_enum_with_truthy([1, 2, 3, 4])
    assert false == RustlerTest.untagged_enum_with_truthy(false)
    assert false == RustlerTest.untagged_enum_with_truthy(nil)
  end

  test "untagged enum for issue 370" do
    assert [1, 2, 3] == RustlerTest.untagged_enum_for_issue_370([1, 2, 3])
  end

  test "newtype tuple" do
    assert {1} == RustlerTest.newtype_echo({1})

    assert_raise ErlangError, "Erlang error: \"Could not decode field 0 on Newtype\"", fn ->
      RustlerTest.newtype_echo({"with error message"})
    end

    assert_raise ArgumentError, fn ->
      RustlerTest.newtype_echo("will result in argument error")
    end
  end

  test "tuplestruct tuple" do
    assert {1, 2, 3} == RustlerTest.tuplestruct_echo({1, 2, 3})

    assert_raise ArgumentError, fn ->
      RustlerTest.tuplestruct_echo({1, 2})
    end

    assert_raise ErlangError, "Erlang error: \"Could not decode field 1 on TupleStruct\"", fn ->
      RustlerTest.tuplestruct_echo({1, "with error message", 3})
    end

    assert_raise ArgumentError, fn ->
      RustlerTest.tuplestruct_echo("will result in argument error")
    end
  end

  test "newtype record" do
    require NewtypeRecord
    value = NewtypeRecord.newtype()
    assert value == RustlerTest.newtype_record_echo(value)

    assert %ErlangError{original: :invalid_record} ==
             assert_raise(ErlangError, fn ->
               RustlerTest.newtype_record_echo({"with error message"})
             end)

    assert_raise ErlangError,
                 "Erlang error: \"Invalid Record structure for NewtypeRecord\"",
                 fn ->
                   RustlerTest.newtype_record_echo("error")
                 end

    assert_raise ErlangError,
                 "Erlang error: \"Could not decode field 0 on Record NewtypeRecord\"",
                 fn ->
                   RustlerTest.newtype_record_echo(NewtypeRecord.newtype(a: "error"))
                 end
  end

  test "tuplestruct record" do
    require TupleStructRecord
    value = TupleStructRecord.tuplestruct()
    assert value == RustlerTest.tuplestruct_record_echo(value)

    assert %ErlangError{original: :invalid_record} ==
             assert_raise(ErlangError, fn -> RustlerTest.tuplestruct_record_echo({"invalid"}) end)

    assert_raise ErlangError,
                 "Erlang error: \"Invalid Record structure for TupleStructRecord\"",
                 fn ->
                   RustlerTest.tuplestruct_record_echo("error")
                 end
  end

  test "reserved keywords" do
    assert %{override: 1} == RustlerTest.reserved_keywords_type_echo(%{override: 1})

    assert %{__struct__: Struct, override: 1} ==
             RustlerTest.reserved_keywords_type_echo(%{__struct__: Struct, override: 1})

    assert {1} == RustlerTest.reserved_keywords_type_echo({1})
    assert {:record, 1} == RustlerTest.reserved_keywords_type_echo({:record, 1})
  end
end
