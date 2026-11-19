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

  test "async_spawned_work demonstrates Caller can be cloned and sent across threads" do
    ref = RustlerTest.async_spawned_work(3)
    assert is_reference(ref)

    # Should receive messages from spawned tasks and final result
    # Each spawned task sends its i value, plus final result
    messages =
      for _ <- 1..4 do
        receive do
          msg -> msg
        after
          500 -> :timeout
        end
      end

    # Check we got messages from spawned tasks (sent across threads)
    assert {ref, 0} in messages
    assert {ref, 1} in messages
    assert {ref, 2} in messages

    # Final result: 0 + 1 + 2 = 3
    assert {ref, 3} in messages
  end

  test "async_channel_echo demonstrates bidirectional communication with Stream trait" do
    # Start the task and get channel sender (which is also the task ref)
    channel_sender = RustlerTest.async_channel_echo()
    assert is_reference(channel_sender)

    # Send messages to the task via the channel
    assert :ok == RustlerTest.channel_send_string(channel_sender, "hello")
    assert :ok == RustlerTest.channel_send_string(channel_sender, "world")
    assert :ok == RustlerTest.channel_send_string(channel_sender, "stop")

    # Collect echo messages
    messages =
      for _ <- 1..3 do
        receive do
          msg -> msg
        after
          500 -> :timeout
        end
      end

    # Check we got echoes (tagged with channel_sender)
    assert {channel_sender, "echo: hello"} in messages
    assert {channel_sender, "echo: world"} in messages

    # Final message with count
    assert {channel_sender, "Received 2 messages"} in messages
  end

  test "stateful_worker demonstrates enum-based commands and responses" do
    # Start the stateful worker
    worker = RustlerTest.stateful_worker()
    assert is_reference(worker)

    # Add 10
    assert :ok == RustlerTest.worker_send_command(worker, {:add, %{value: 10}})

    assert_receive {^worker, {:updated, %{old_value: 0, new_value: 10}}}, 500

    # Add 5
    assert :ok == RustlerTest.worker_send_command(worker, {:add, %{value: 5}})

    assert_receive {^worker, {:updated, %{old_value: 10, new_value: 15}}}, 500

    # Multiply by 2
    assert :ok == RustlerTest.worker_send_command(worker, {:multiply, %{value: 2}})

    assert_receive {^worker, {:updated, %{old_value: 15, new_value: 30}}}, 500

    # Subtract 5
    assert :ok == RustlerTest.worker_send_command(worker, {:subtract, %{value: 5}})

    assert_receive {^worker, {:updated, %{old_value: 30, new_value: 25}}}, 500

    # Get current value
    assert :ok == RustlerTest.worker_send_command(worker, :get_current)

    assert_receive {^worker, {:current, %{value: 25}}}, 500

    # Reset
    assert :ok == RustlerTest.worker_send_command(worker, :reset)

    assert_receive {^worker, :reset}, 500

    # Verify reset worked
    assert :ok == RustlerTest.worker_send_command(worker, :get_current)

    assert_receive {^worker, {:current, %{value: 0}}}, 500

    # Shutdown
    assert :ok == RustlerTest.worker_send_command(worker, :shutdown)

    # Should receive two shutdown messages: one from the command, one as final
    assert_receive {^worker, {:shutting_down, %{final_value: 0, operations: 0}}}, 500
    assert_receive {^worker, {:shutting_down, %{final_value: 0, operations: 0}}}, 500
  end
end
