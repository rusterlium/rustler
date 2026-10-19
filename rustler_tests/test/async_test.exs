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

  test "terms and new allocations survive repeated yields" do
    term = %{values: Enum.to_list(1..100), binary: :binary.copy("abc", 100)}

    for yields <- [0, 1, 100] do
      assert RustlerTest.async_echo(term, yields) == {term, [1, 2, 3]}
    end
  end

  test "borrowed input remains valid after yielding" do
    text = :binary.copy("borrowed", 100)
    assert RustlerTest.async_borrowed(text) == text
  end

  test "immediate, renamed and dirty async NIFs" do
    assert RustlerTest.async_immediate(41) == 42
    assert RustlerTest.async_dirty(41) == 42
  end

  test "decode failures, errors and exceptions" do
    assert_raise ArgumentError, fn -> RustlerTest.async_borrowed(:not_a_binary) end
    assert_raise ArgumentError, fn -> RustlerTest.async_error(false) end
    assert catch_error(RustlerTest.async_error(true)) == "async error"
  end

  test "panics before and after yielding are contained" do
    for yields <- [0, 1, 10] do
      assert catch_error(RustlerTest.async_panic(yields)) == :nif_panicked
    end
  end

  test "unsupported pending futures fail instead of spinning" do
    assert catch_error(RustlerTest.async_pending()) == :async_nif_pending_without_yield
    assert catch_error(RustlerTest.async_self_waking()) == :async_nif_pending_without_yield
    assert catch_error(RustlerTest.async_executor_pending()) == :async_nif_pending_without_yield

    assert catch_error(RustlerTest.async_unwrapped_executor()) ==
             "on_executor requires #[rustler::nif(executor)]"
  end

  test "external timers interleave with cooperative yields and preserve terms" do
    term = %{values: Enum.to_list(1..100), binary: :binary.copy("abc", 100)}

    for waits <- [0, 1, 20] do
      assert RustlerTest.async_executor_echo(term, waits) == {term, [1, 2, 3]}
    end
  end

  test "fast external completions do not lose wakeups or leave messages" do
    assert RustlerTest.async_executor_immediate(1000) == 1000
    refute_receive {:rustler_async_ready, _}, 10
  end

  test "external socket IO executes under its runtime, including renamed dirty NIFs" do
    text = :binary.copy("socket echo", 100)
    assert RustlerTest.async_executor_io(text) == text
  end

  test "external errors and post-wait exceptions propagate" do
    assert catch_error(RustlerTest.async_executor_error(0)) ==
             "executor rejected task: test rejection"

    assert catch_error(RustlerTest.async_executor_error(1)) ==
             "executor stopped before task completed"

    assert catch_error(RustlerTest.async_executor_error(2)) == "executor task panicked"
    assert catch_error(RustlerTest.async_executor_error(3)) == "after executor"
    assert_raise ArgumentError, fn -> RustlerTest.async_executor_error(4) end
    assert_raise ArgumentError, fn -> RustlerTest.async_executor_io(:invalid) end
  end

  test "waiting process is suspended and unrelated mailbox messages are preserved" do
    parent = self()

    pid =
      spawn(fn ->
        RustlerTest.async_executor_sleep(500)
        send(parent, :finished)
      end)

    Process.sleep(30)
    assert {:status, :waiting} = Process.info(pid, :status)
    send(self(), :unrelated)
    assert RustlerTest.async_executor_sleep(1) == {}
    assert_receive :unrelated
    assert_receive :finished, 1000
  end

  test "user results cannot be confused with internal wait tokens" do
    for term <- [
          {:rustler_async_wait, make_ref(), :fake_resource},
          {:rustler_async_done, 42}
        ] do
      assert RustlerTest.async_executor_identity(term) == term
    end
  end

  test "termination cancels both the NIF future and its external task" do
    tracker = RustlerTest.async_tracker()
    {pid, monitor} = spawn_monitor(fn -> RustlerTest.async_executor_forever(tracker) end)
    await_state(tracker, {true, false})
    Process.exit(pid, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^pid, :killed}
    await_state(tracker, {true, true})
    await_external_drop(tracker)
  end

  test "continuation tokens cannot be resumed by a different process" do
    parent = self()

    {pid, monitor} =
      spawn_monitor(fn ->
        {:rustler_async_wait, reference, resource} =
          RustlerTest.__rustler_async_async_executor_sleep(100)

        send(parent, {:token, resource})

        receive do
          {:rustler_async_ready, ^reference} ->
            assert RustlerTest.__rustler_async_continue(resource) == {:rustler_async_done, {}}
        end
      end)

    assert_receive {:token, resource}
    assert_raise ArgumentError, fn -> RustlerTest.__rustler_async_continue(resource) end
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}, 1000
  end

  defp await_external_drop(tracker, attempts \\ 100)

  defp await_external_drop(tracker, 0),
    do: assert(RustlerTest.async_external_dropped(tracker))

  defp await_external_drop(tracker, attempts) do
    unless RustlerTest.async_external_dropped(tracker) do
      Process.sleep(10)
      await_external_drop(tracker, attempts - 1)
    end
  end

  test "destructor panics on completion and pending cleanup are contained" do
    for pending <- [false, true] do
      assert catch_error(RustlerTest.async_drop_panic(pending)) == :nif_panicked
    end
  end

  test "terminating a yielding process drops its future" do
    tracker = RustlerTest.async_tracker()
    {pid, monitor} = spawn_monitor(fn -> RustlerTest.async_forever(tracker) end)
    await_state(tracker, {true, false})

    Process.exit(pid, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^pid, :killed}
    await_state(tracker, {true, true})
  end

  defp await_state(tracker, expected, attempts \\ 100)

  defp await_state(tracker, expected, 0),
    do: assert(RustlerTest.async_tracker_state(tracker) == expected)

  defp await_state(tracker, expected, attempts) do
    if RustlerTest.async_tracker_state(tracker) != expected do
      Process.sleep(10)
      await_state(tracker, expected, attempts - 1)
    end
  end
end
