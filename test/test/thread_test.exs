defmodule RustlerTest.ThreadTest do
  use ExUnit.Case, async: true

  test "simple threaded nif" do
    RustlerTest.threaded_fac 19
    receive do
      x -> assert x == 121645100408832000
    end
  end

  test "sleeping nif" do
    RustlerTest.threaded_sleep 200
    receive do
      _ -> raise :timeout_expected
    after 100 ->
        nil
    end

    receive do
      x -> assert x == 200
    after 1000 ->
        raise :message_expected
    end
  end

  test "many threads" do
    # Spawn 50 threads.
    times = Enum.map(1..50, fn (x) -> x * 10 end)
    Enum.map(times, &RustlerTest.threaded_sleep/1)

    # Wait for them all to respond.
    results = Enum.map(times, fn (_) ->
      receive do
        y -> y
      after 1000 ->
          :timeout
      end
    end)

    # The OS scheduler guarantees virtually nothing about sleep().
    # Answers may arrive out of order.
    assert Enum.sort(results) == times
  end

  test "thread panic" do
    RustlerTest.threaded_fac 100  # overflows u64 and panics
    receive do
      msg -> assert msg == {:error, "threaded_fac: integer overflow"}
    end
  end
end
