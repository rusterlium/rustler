defmodule RustlerTest.TupleTest do
  use ExUnit.Case, async: true

  test "sum from two integer in a tuple" do
    assert RustlerTest.add_from_tuple({1, 2}) == 3
    assert RustlerTest.add_from_tuple({15, 4}) == 19
    assert RustlerTest.add_from_tuple({-56, 7}) == -49
  end

  test "add one on each element of a tuple" do
    assert RustlerTest.add_one_to_tuple({1, 2}) == {2, 3}
    assert RustlerTest.add_one_to_tuple({15, 4}) == {16, 5}
  end

  test "join two strings from tuple" do
    assert RustlerTest.join_tuple_elements({"rus", "tler"}) == "rustler"
    assert RustlerTest.join_tuple_elements({"eli", "xir"}) == "elixir"
  end

  test "add one to elements of tuple if present" do
    assert RustlerTest.maybe_add_one_to_tuple({1, 2}) == {2, 3}
    assert RustlerTest.maybe_add_one_to_tuple({41, 19}) == {42, 20}

    assert RustlerTest.maybe_add_one_to_tuple(nil) == nil
  end

  test "sum from two i32 numbers and return result" do
    assert RustlerTest.add_i32_from_tuple({9, 1}) == {:ok, 10}
    assert RustlerTest.add_i32_from_tuple({2_147_483_646, 1}) == {:ok, 2_147_483_647}

    assert RustlerTest.add_i32_from_tuple({1, 2_147_483_647}) ==
             {:error,
              "Cannot sum 1 + 2147483647 because the result is bigger than an i32 number."}
  end

  test "mix two types in a tuple" do
    assert RustlerTest.greeting_person_from_tuple({22, "Alice"}) ==
             "Hello, Alice! You are allowed in the bar area."

    assert RustlerTest.greeting_person_from_tuple({16, "Bob"}) ==
             "Hi, Bob! I'm sorry, but you are not allowed in the bar area."

    assert catch_error(RustlerTest.greeting_person_from_tuple("Godzilla")) == :badarg
  end
end
