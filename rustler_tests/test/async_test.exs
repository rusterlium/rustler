defmodule RustlerTest.AsyncTest do
  use ExUnit.Case, async: false

  test "async_add returns ref and result comes via message" do
    ref = RustlerTest.async_add(10, 20)
    assert is_reference(ref)

    assert_receive {^ref, result}, 1000
    assert result == 30
  end

  test "async_sleep_and_return" do
    ref = RustlerTest.async_sleep_and_return(50, "hello world")
    assert is_reference(ref)

    assert_receive {^ref, result}, 1000
    assert result == "hello world"
  end

  test "async_tuple_multiply" do
    ref = RustlerTest.async_tuple_multiply({6, 7})
    assert is_reference(ref)

    assert_receive {^ref, result}, 1000
    assert result == 42
  end

  test "multiple async calls can run concurrently" do
    # Start 3 async operations
    ref1 = RustlerTest.async_sleep_and_return(100, "first")
    ref2 = RustlerTest.async_sleep_and_return(100, "second")
    ref3 = RustlerTest.async_sleep_and_return(100, "third")

    assert is_reference(ref1)
    assert is_reference(ref2)
    assert is_reference(ref3)

    # Collect all results
    results =
      for _ <- 1..3 do
        receive do
          {_ref, msg} -> msg
        after
          1000 -> :timeout
        end
      end

    # All should have completed
    assert "first" in results
    assert "second" in results
    assert "third" in results
  end

  test "async_with_progress sends intermediate messages using Caller" do
    ref = RustlerTest.async_with_progress(3)
    assert is_reference(ref)

    # All messages (intermediate and final) are tagged with the ref and have same type (i64)
    # Should receive: {ref, 0}, {ref, 1}, {ref, 2}, {ref, 3}
    # Final result: {ref, 3} (which is 0 + 1 + 2)

    messages =
      for _ <- 1..4 do
        receive do
          msg -> msg
        after
          500 -> :timeout
        end
      end

    # Check we got progress updates (intermediate i64 values) tagged with ref
    assert {ref, 0} in messages
    assert {ref, 1} in messages
    assert {ref, 2} in messages

    # Final result should also be tagged with ref: {ref, 3}
    assert {ref, 3} in messages
  end
end
