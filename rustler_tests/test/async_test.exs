defmodule RustlerTest.AsyncTest do
  use ExUnit.Case, async: false

  test "async_add returns :ok and result comes via message" do
    assert :ok == RustlerTest.async_add(10, 20)

    assert_receive result, 1000
    assert result == 30
  end

  test "async_sleep_and_return" do
    assert :ok == RustlerTest.async_sleep_and_return(50, "hello world")

    assert_receive result, 1000
    assert result == "hello world"
  end

  test "async_tuple_multiply" do
    assert :ok == RustlerTest.async_tuple_multiply({6, 7})

    assert_receive result, 1000
    assert result == 42
  end

  test "multiple async calls can run concurrently" do
    # Start 3 async operations
    assert :ok == RustlerTest.async_sleep_and_return(100, "first")
    assert :ok == RustlerTest.async_sleep_and_return(100, "second")
    assert :ok == RustlerTest.async_sleep_and_return(100, "third")

    # Collect all results
    results =
      for _ <- 1..3 do
        receive do
          msg -> msg
        after
          1000 -> :timeout
        end
      end

    # All should have completed
    assert "first" in results
    assert "second" in results
    assert "third" in results
  end

  test "async_with_progress sends intermediate messages using CallerPid" do
    assert :ok == RustlerTest.async_with_progress(3)

    # Should receive progress messages: {:progress, 0}, {:progress, 1}, {:progress, 2}
    # Then final result: 3 (which is 0 + 1 + 2)

    messages =
      for _ <- 1..4 do
        receive do
          msg -> msg
        after
          500 -> :timeout
        end
      end

    # Check we got progress updates
    assert {"progress", 0} in messages
    assert {"progress", 1} in messages
    assert {"progress", 2} in messages

    # Final result should be sum: 0 + 1 + 2 = 3
    assert 3 in messages
  end
end
