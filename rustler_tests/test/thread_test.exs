defmodule RustlerTest.ThreadTest do
  use ExUnit.Case, async: true

  test "simple threaded nif" do
    RustlerTest.threaded_fac(19)

    receive do
      x -> assert x == 121_645_100_408_832_000
    end
  end

  test "sleeping nif" do
    start = System.monotonic_time(:millisecond)
    RustlerTest.threaded_sleep(200)

    receive do
      {:threaded_sleep, x} ->
        elapsed = System.monotonic_time(:millisecond) - start
        assert x == 200
        assert elapsed >= 200
    after
      1500 ->
        raise "message_expected"
    end
  end

  test "many threads" do
    # Spawn 50 threads.
    times = Enum.map(1..50, fn x -> x * 10 end)
    Enum.map(times, &RustlerTest.threaded_sleep/1)

    # Wait for them all to respond.
    results =
      Enum.map(times, fn _ ->
        receive do
          {:threaded_sleep, y} -> y
        after
          1000 ->
            :timeout
        end
      end)

    # The OS scheduler guarantees virtually nothing about sleep().
    # Answers may arrive out of order.
    assert Enum.sort(results) == times
  end

  test "thread panic" do
    # Overflows u64 and panics.
    #
    # Note that we are suppressing the panic message inside the
    # function implementation.
    RustlerTest.threaded_fac(100)

    receive do
      msg -> assert msg == {:error, "threaded_fac: integer overflow"}
    end
  end
end
