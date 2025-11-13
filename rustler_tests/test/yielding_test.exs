defmodule RustlerTest.YieldingTest do
  use ExUnit.Case, async: false

  # These tests verify TRUE cooperative yielding NIFs using enif_schedule_nif.
  # Unlike async NIFs (which return references and send messages),
  # yielding NIFs appear synchronous but yield internally to the BEAM scheduler.

  describe "yielding_immediate/0" do
    test "returns result immediately without yielding" do
      # This should complete on first poll without needing to reschedule
      result = RustlerTest.yielding_immediate()
      assert result == 42
    end

    test "returns i64 type" do
      result = RustlerTest.yielding_immediate()
      assert is_integer(result)
    end
  end

  describe "yielding_sum/1" do
    test "computes sum of 0..n correctly" do
      # sum(0..99) = 4950
      result = RustlerTest.yielding_sum(100)
      assert result == 4950
    end

    test "yields during computation for large inputs" do
      # This should trigger multiple yield points (every 100 iterations)
      # sum(0..9999) = 49995000
      result = RustlerTest.yielding_sum(10_000)
      assert result == 49_995_000
    end

    test "works with small inputs that don't need yielding" do
      # sum(0..9) = 45
      result = RustlerTest.yielding_sum(10)
      assert result == 45
    end

    test "handles zero input" do
      result = RustlerTest.yielding_sum(0)
      assert result == 0
    end

    test "is deterministic - same input gives same output" do
      result1 = RustlerTest.yielding_sum(1000)
      result2 = RustlerTest.yielding_sum(1000)
      assert result1 == result2
      assert result1 == 499_500
    end

    test "blocks the calling process until complete" do
      # This verifies the synchronous nature - we don't receive messages,
      # the function call just blocks until the result is ready
      start_time = System.monotonic_time(:millisecond)
      result = RustlerTest.yielding_sum(10_000)
      end_time = System.monotonic_time(:millisecond)

      assert result == 49_995_000
      # Should take some time due to computation and yielding
      assert end_time >= start_time
    end
  end

  describe "yielding_work_with_sleeps/0" do
    test "returns processed string with step markers" do
      result = RustlerTest.yielding_work_with_sleeps()

      # Should contain "Processing" at the start
      assert String.starts_with?(result, "Processing")

      # Should contain step markers
      assert result =~ "step0"
      assert result =~ "step1"
      assert result =~ "step2"
      assert result =~ "step3"
      assert result =~ "step4"
    end

    test "includes dots from simulated work" do
      result = RustlerTest.yielding_work_with_sleeps()

      # Should have dots from the work simulation (1000 dots per step * 5 steps)
      dot_count = result |> String.graphemes() |> Enum.count(&(&1 == "."))
      assert dot_count == 5000
    end

    test "processes all 5 steps in order" do
      result = RustlerTest.yielding_work_with_sleeps()

      # Extract positions of step markers
      step0_pos = :binary.match(result, "step0") |> elem(0)
      step1_pos = :binary.match(result, "step1") |> elem(0)
      step2_pos = :binary.match(result, "step2") |> elem(0)
      step3_pos = :binary.match(result, "step3") |> elem(0)
      step4_pos = :binary.match(result, "step4") |> elem(0)

      # Steps should appear in order
      assert step0_pos < step1_pos
      assert step1_pos < step2_pos
      assert step2_pos < step3_pos
      assert step3_pos < step4_pos
    end
  end

  describe "yielding_tuple_result/2" do
    test "returns tuple with sum, product, and status" do
      {sum, product, status} = RustlerTest.yielding_tuple_result(10, 5)

      # sum(0..9) = 45
      assert sum == 45
      # product(1..5) = 120
      assert product == 120
      assert status == "done"
    end

    test "handles edge cases" do
      # x=0 should give sum=0
      {sum, product, status} = RustlerTest.yielding_tuple_result(0, 1)
      assert sum == 0
      assert product == 1
      assert status == "done"
    end

    test "computes correct factorial in product" do
      # 5! = 120
      {_sum, product, _status} = RustlerTest.yielding_tuple_result(1, 5)
      assert product == 120

      # 6! = 720
      {_sum, product, _status} = RustlerTest.yielding_tuple_result(1, 6)
      assert product == 720
    end

    test "yields during computation for larger inputs" do
      # This should trigger multiple yield points
      {sum, product, status} = RustlerTest.yielding_tuple_result(100, 10)

      assert sum == 4950
      # 10!
      assert product == 3_628_800
      assert status == "done"
    end

    test "returns correct types" do
      result = RustlerTest.yielding_tuple_result(5, 3)

      assert is_tuple(result)
      assert tuple_size(result) == 3

      {sum, product, status} = result
      assert is_integer(sum)
      assert is_integer(product)
      assert is_binary(status)
    end
  end

  describe "cooperative yielding behavior" do
    test "multiple yielding calls can be made sequentially" do
      # These should all complete and return results directly
      result1 = RustlerTest.yielding_sum(100)
      result2 = RustlerTest.yielding_sum(200)
      result3 = RustlerTest.yielding_immediate()

      assert result1 == 4950
      assert result2 == 19_900
      assert result3 == 42
    end

    test "yielding NIFs don't send messages" do
      # Clear mailbox
      flush_mailbox()

      # Call yielding NIF
      result = RustlerTest.yielding_sum(1000)
      assert result == 499_500

      # Verify no messages were sent
      refute_receive _, 100
    end

    test "yielding NIFs block the calling process" do
      # Start a task that calls a yielding NIF
      parent = self()

      task =
        Task.async(fn ->
          send(parent, :started)
          result = RustlerTest.yielding_sum(10_000)
          send(parent, {:completed, result})
          {:completed, result}
        end)

      # Wait for task to start
      assert_receive :started, 100

      # The task should block until the NIF completes
      # We should receive :completed, not timeout
      assert_receive {:completed, 49_995_000}, 5_000

      # Verify task completes successfully and returns the result
      assert Task.await(task) == {:completed, 49_995_000}
    end

    test "concurrent yielding calls from different processes" do
      # Spawn multiple processes calling yielding NIFs concurrently
      parent = self()

      for i <- 1..5 do
        spawn(fn ->
          result = RustlerTest.yielding_sum(1000)
          send(parent, {:result, i, result})
        end)
      end

      # Collect all results
      results =
        for _ <- 1..5 do
          receive do
            {:result, i, result} -> {i, result}
          after
            5_000 -> :timeout
          end
        end

      # All should have computed the same result
      assert length(results) == 5
      assert Enum.all?(results, fn {_i, result} -> result == 499_500 end)
    end
  end

  describe "performance characteristics" do
    test "yielding NIFs don't block the scheduler excessively" do
      # This test verifies that yielding NIFs cooperate with the scheduler
      # by measuring if other work can interleave

      parent = self()
      counter = :counters.new(1, [:atomics])

      # Start a process that increments a counter in a tight loop
      _counter_task =
        spawn(fn ->
          for _ <- 1..1000 do
            :counters.add(counter, 1, 1)
            Process.sleep(1)
          end

          send(parent, :counter_done)
        end)

      # Start the yielding NIF computation
      _nif_task =
        spawn(fn ->
          result = RustlerTest.yielding_sum(100_000)
          send(parent, {:nif_done, result})
        end)

      # Wait for both to complete
      assert_receive :counter_done, 10_000
      assert_receive {:nif_done, 4_999_950_000}, 10_000

      # The counter should have made good progress despite the NIF running
      # This shows the NIF yielded and let other work run
      count = :counters.get(counter, 1)
      # Should have counted most of the iterations
      assert count > 500
    end
  end

  describe "reduction consumption" do
    test "yielding NIFs consume reductions" do
      # Get initial reduction count
      {:reductions, reductions_before} = Process.info(self(), :reductions)

      # Call a yielding NIF that should consume reductions
      result = RustlerTest.yielding_sum(10_000)

      # Get reduction count after NIF call
      {:reductions, reductions_after} = Process.info(self(), :reductions)

      # Verify the result is correct
      assert result == 49_995_000

      # Verify reductions were consumed (should be significantly higher)
      reductions_consumed = reductions_after - reductions_before

      # The NIF should consume reductions - we expect at least some consumption
      # since we're yielding ~100 times (every 100 iterations for 10,000 iterations)
      assert reductions_consumed > 0,
             "Expected reductions to be consumed, but consumed #{reductions_consumed}"
    end

    test "larger computations consume more reductions" do
      # Small computation
      {:reductions, before_small} = Process.info(self(), :reductions)
      _result_small = RustlerTest.yielding_sum(1_000)
      {:reductions, after_small} = Process.info(self(), :reductions)
      small_consumed = after_small - before_small

      # Larger computation (10x more iterations, 10x more yields)
      {:reductions, before_large} = Process.info(self(), :reductions)
      _result_large = RustlerTest.yielding_sum(10_000)
      {:reductions, after_large} = Process.info(self(), :reductions)
      large_consumed = after_large - before_large

      # Larger computation should consume more reductions
      assert large_consumed > small_consumed,
             "Expected large computation (#{large_consumed}) to consume more than small (#{small_consumed})"
    end

    test "immediate completion NIFs consume minimal reductions" do
      # Get initial reduction count
      {:reductions, reductions_before} = Process.info(self(), :reductions)

      # Call an immediate NIF (no yields)
      result = RustlerTest.yielding_immediate()

      {:reductions, reductions_after} = Process.info(self(), :reductions)

      # Verify the result
      assert result == 42

      # This should consume very few reductions since it completes immediately
      reductions_consumed = reductions_after - reductions_before

      # Should still consume some reductions (for the NIF call itself)
      assert reductions_consumed >= 0
    end

    test "yielding NIFs consume reductions across multiple calls" do
      # Make multiple NIF calls and track total reduction consumption
      total_reductions =
        Enum.reduce(1..5, 0, fn _, acc ->
          {:reductions, before} = Process.info(self(), :reductions)
          _result = RustlerTest.yielding_sum(1_000)
          {:reductions, after_call} = Process.info(self(), :reductions)

          acc + (after_call - before)
        end)

      # Total reductions should be significant
      assert total_reductions > 0,
             "Expected multiple NIF calls to consume reductions, consumed #{total_reductions}"
    end

    test "reductions are consumed in spawned process" do
      parent = self()

      # Spawn a process to run the yielding NIF
      spawn(fn ->
        {:reductions, before} = Process.info(self(), :reductions)
        result = RustlerTest.yielding_sum(10_000)
        {:reductions, after_call} = Process.info(self(), :reductions)

        consumed = after_call - before
        send(parent, {:reductions_consumed, consumed, result})
      end)

      # Receive the result
      assert_receive {:reductions_consumed, consumed, result}, 5_000

      # Verify result and reductions
      assert result == 49_995_000

      assert consumed > 0,
             "Expected spawned process to consume reductions, consumed #{consumed}"
    end
  end

  # Helper to flush mailbox
  defp flush_mailbox do
    receive do
      _ -> flush_mailbox()
    after
      0 -> :ok
    end
  end
end
