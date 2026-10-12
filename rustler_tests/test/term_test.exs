defmodule RustlerTest.TermTest do
  use ExUnit.Case, async: true

  test "term_debug" do
    assert RustlerTest.term_debug(33) == "33"
    assert RustlerTest.term_debug("hello world") == "<<\"hello world\">>"
    assert RustlerTest.term_debug("饂") == "<<233,165,130>>"
    assert RustlerTest.term_debug({:atom, :pair}) == "{atom,pair}"

    range = 0..1000

    assert RustlerTest.term_debug_and_reparse(range) == range

    assert RustlerTest.term_debug(Enum.to_list(0..5)) == "[0,1,2,3,4,5]"
    assert RustlerTest.term_debug(Enum.to_list(0..1000)) == "[#{Enum.join(0..1000, ",")}]"
    assert RustlerTest.term_debug([[[[]], []], [[]], []]) == "[[[[]],[]],[[]],[]]"

    sues = Enum.map(1..500, fn i -> %{name: "Aunt Sue", id: i} end)

    sues_debug =
      Enum.map(1..500, fn i -> RustlerTest.term_debug_and_reparse(%{name: "Aunt Sue", id: i}) end)

    assert sues == sues_debug
  end

  test "term equality" do
    assert RustlerTest.term_eq(1, 1)
    refute RustlerTest.term_eq(1.0, 1)

    assert RustlerTest.term_eq("something", "something")
    refute RustlerTest.term_eq("something", "else")

    assert RustlerTest.term_eq({1, 0}, {1, 0})
    refute RustlerTest.term_eq({0, 1}, {1, 0})
  end

  test "term compare" do
    # Number ordering
    assert RustlerTest.term_cmp(5, 10) == :less
    assert RustlerTest.term_cmp(10, 5) == :greater
    assert RustlerTest.term_cmp(5, 5) == :equal
    assert RustlerTest.term_cmp(5, 5.0) == :equal

    # Other term types
    assert RustlerTest.term_cmp(5, :test) == :less
  end

  test "term hash" do
    assert RustlerTest.term_phash2_hash(:foobar) == :erlang.phash2(:foobar)
    assert RustlerTest.term_phash2_hash("testing") == :erlang.phash2("testing")
    assert RustlerTest.term_phash2_hash(42) == :erlang.phash2(42)

    # Assume a certain distribution
    unique =
      0..100
      |> Enum.map(&RustlerTest.term_phash2_hash(&1))
      |> Enum.group_by(fn n -> n end, fn n -> n end)
      |> map_size

    assert unique > 50

    unique =
      0..100
      |> Enum.map(&RustlerTest.term_internal_hash(&1, 0))
      |> Enum.group_by(fn n -> n end, fn n -> n end)
      |> map_size

    assert unique > 50
  end

  test "term type" do
    assert RustlerTest.term_type(:foo) == :atom
    assert RustlerTest.term_type("foo") == :binary
    assert RustlerTest.term_type(42.2) == :float
    assert RustlerTest.term_type(42) == :integer
    assert RustlerTest.term_type(%{}) == :map
    assert RustlerTest.term_type([]) == :list
    assert RustlerTest.term_type({:ok, 42}) == :tuple
    assert RustlerTest.term_type(self()) == :pid
    assert RustlerTest.term_type(& &1) == :fun
    assert RustlerTest.term_type(make_ref()) == :reference
  end

  if RustlerTest.Helper.has_nif_version("2.18") do
    test "term size" do
      assert RustlerTest.term_size(%{}) > 0
      assert RustlerTest.term_size(%{a: 1, c: 2}) > RustlerTest.term_size(%{})
    end
  end

  test "raw term wrappers roundtrip and reject mismatched types" do
    port = Port.open({:spawn_executable, System.find_executable("cat")}, [])

    try do
      cases = [
        atom: :example,
        binary: "hello",
        binary: <<>>,
        float: 1.5,
        fun: fn x -> x end,
        integer: 42,
        integer: Integer.pow(2, 128),
        list: [],
        list: [1, 2],
        list: [1 | :tail],
        local_pid: self(),
        map: %{key: :value},
        pid: self(),
        port: port,
        reference: make_ref(),
        tuple: {},
        tuple: {:ok, 42}
      ]

      for {kind, value} <- cases do
        assert RustlerTest.raw_term_roundtrip(kind, value) === value
        wrong_type = if kind == :atom, do: 42, else: :not_this_type
        assert_raise ArgumentError, fn -> RustlerTest.raw_term_roundtrip(kind, wrong_type) end
      end
    after
      Port.close(port)
    end
  end

  if RustlerTest.Helper.has_nif_version("2.15") do
    test "bitstrings include binaries but binary wrappers reject unaligned bitstrings" do
      for value <- ["hello", <<>>, <<1::size(1)>>] do
        assert RustlerTest.raw_term_roundtrip(:bitstring, value) === value
      end

      assert_raise ArgumentError, fn -> RustlerTest.raw_term_roundtrip(:bitstring, :atom) end

      assert_raise ArgumentError, fn ->
        RustlerTest.raw_term_roundtrip(:binary, <<1::size(1)>>)
      end
    end
  end

  test "general pids accept remote pids but free local pids reject them" do
    remote_pid =
      :erlang.binary_to_term(<<131, 88, 119, 14, "remote@invalid", 0::32, 0::32, 1::32>>)

    assert RustlerTest.raw_term_roundtrip(:pid, remote_pid) === remote_pid
    assert_raise ArgumentError, fn -> RustlerTest.raw_term_roundtrip(:local_pid, remote_pid) end
  end

  test "free terms survive clearing and dropping their source environment" do
    assert RustlerTest.free_term_after_clear(:atom, :example) == :example
    assert RustlerTest.free_term_after_clear(:local_pid, self()) == self()
  end

  test "encoded bound terms survive clearing and dropping their source environment" do
    value = {%{binary: :binary.copy("payload", 100)}, [1, 2, 3], make_ref(), Integer.pow(2, 128)}
    assert RustlerTest.raw_bound_copy(value) === value
    assert RustlerTest.raw_bound_copy({}) === {}
  end

  test "typed map operations preserve values and missing-key behavior" do
    map = %{a: 1, b: 2}
    operation = fn op, key, value -> RustlerTest.typed_map_operation(op, map, key, value) end
    assert operation.("get", :a, nil) == 1
    assert operation.("size", nil, nil) == 2
    assert operation.("put", :c, 3) == %{a: 1, b: 2, c: 3}
    assert operation.("put", :a, 3) == %{a: 3, b: 2}
    assert operation.("update", :a, 3) == %{a: 3, b: 2}
    assert operation.("remove", :a, nil) == %{b: 2}
    assert operation.("remove", :missing, nil) == map
    assert Map.new(operation.("entries", nil, nil)) == map
    assert RustlerTest.typed_map_operation("size", %{}, nil, nil) == 0
    assert RustlerTest.typed_map_operation("entries", %{}, nil, nil) == []
    assert_raise ArgumentError, fn -> operation.("get", :missing, nil) end
    assert_raise ArgumentError, fn -> operation.("update", :missing, 3) end
  end

  test "typed map constructors copy foreign inputs and reject different array lengths" do
    map = %{"a" => ["one"], "b" => {"two", 2}}
    assert RustlerTest.typed_map_build(["a", "b"], [["one"], {"two", 2}]) == {%{}, map, map, map}
    assert RustlerTest.typed_map_build([], []) == {%{}, %{}, %{}, %{}}
    assert_raise ArgumentError, fn -> RustlerTest.typed_map_build([:a], []) end
  end

  test "typed list operations handle empty and improper lists" do
    operation = fn op, list -> RustlerTest.typed_list_operation(op, list, 0) end
    assert operation.("empty", [1]) == []
    assert operation.("length", [1, 2]) == 2
    assert operation.("length", []) == 0
    assert operation.("cell", [1, 2]) == {1, [2]}
    assert operation.("cell", [1 | :tail]) == {1, :tail}
    assert operation.("reverse", [1, 2]) == [2, 1]
    assert operation.("reverse", []) == []
    assert operation.("prepend", [1, 2]) == [0, 1, 2]
    assert operation.("prepend", [1 | :tail]) == [0, 1 | :tail]
    assert operation.("entries", [1, 2]) == [1, 2]
    assert operation.("entries", []) == []
    assert_raise ArgumentError, fn -> operation.("cell", []) end
    assert_raise ArgumentError, fn -> operation.("length", [1 | :tail]) end
    assert_raise ArgumentError, fn -> operation.("reverse", [1 | :tail]) end
  end

  test "Term list adapters retain errors and arbitrary prepend tails" do
    for {operation, list} <- [
          {"length", []},
          {"length", [1, 2]},
          {"cell", [1 | :tail]},
          {"reverse", [1, 2]},
          {"prepend", [1 | :tail]}
        ] do
      assert RustlerTest.term_list_operation(operation, list, 0) ==
               RustlerTest.typed_list_operation(operation, list, 0)
    end

    assert RustlerTest.term_list_operation("prepend", :tail, 0) == [0 | :tail]

    for operation <- ["length", "cell", "reverse"] do
      assert_raise ArgumentError, fn ->
        RustlerTest.term_list_operation(operation, :not_a_list, 0)
      end
    end
  end

  test "typed atom, binary, and tuple methods reuse their existing views" do
    tuple = {["one"], %{two: "three"}}

    assert RustlerTest.typed_term_views(:example, <<1, 2, 255>>, tuple) ==
             {"example", <<1, 2, 255>>, Tuple.to_list(tuple), tuple}

    assert RustlerTest.typed_term_views(:example, <<>>, {}) == {"example", <<>>, [], {}}
  end

  test "safe typed operations and iterators survive clearing their source environment" do
    value = %{nested: [:binary.copy("payload", 100)]}
    assert RustlerTest.typed_map_operation("get", %{key: value}, :key, nil) == value
    {entries, elements} = RustlerTest.typed_iterators_after_clear(%{key: value}, [value])
    assert Map.new(entries) == %{key: value}
    assert elements == [value]
  end

  test "TypedTerm classifies and roundtrips every available wrapper" do
    port = Port.open({:spawn_executable, System.find_executable("cat")}, [])

    remote_pid =
      :erlang.binary_to_term(<<131, 88, 119, 14, "remote@invalid", 0::32, 0::32, 1::32>>)

    try do
      cases = [
        atom: :example,
        atom: nil,
        binary: <<>>,
        binary: :binary.copy("payload", 100),
        float: 1.5,
        fun: fn x -> x + 1 end,
        integer: -42,
        integer: Integer.pow(2, 128),
        list: [],
        list: [%{key: ["value"]}],
        list: [1 | :tail],
        local_pid: self(),
        map: %{key: ["value"]},
        pid: remote_pid,
        port: port,
        reference: make_ref(),
        tuple: {},
        tuple: {:ok, ["value"]}
      ]

      for {kind, value} <- cases do
        assert RustlerTest.typed_term_roundtrip(value) === {kind, value}
      end
    after
      Port.close(port)
    end
  end

  if RustlerTest.Helper.has_nif_version("2.15") do
    test "TypedTerm distinguishes binary and non-byte-aligned bitstrings" do
      assert RustlerTest.typed_term_roundtrip(<<1, 2>>) == {:binary, <<1, 2>>}
      assert RustlerTest.typed_term_roundtrip(<<1::size(1)>>) == {:bitstring, <<1::size(1)>>}
    end
  end
end
