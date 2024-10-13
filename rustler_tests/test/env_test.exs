defmodule RustlerTest.EnvTest do
  use ExUnit.Case, async: true

  test "send" do
    # Start 4 processes. Each one waits for one message,
    # then sends a tuple back to this process.
    parent = self()

    child_pids =
      for _ <- 1..4 do
        spawn(fn ->
          receive do
            # Send the message itself, plus this process's pid.
            msg -> send(parent, {self(), msg})
          end
        end)
      end

    # Send all 4 child processes a message.
    RustlerTest.send_all(child_pids, {:hello, :world})

    # Wait for all 4 processes to send something back.
    results =
      for _ <- 1..4 do
        receive do
          # Collect the pids that the child processes send back.
          {child_pid, {:hello, :world}} -> child_pid
          _ -> raise "fail"
        end
      end

    # Each child should have sent back one result. (Sort before testing
    # because results come back in arbitrary order.)
    assert Enum.sort(results) == Enum.sort(child_pids)
  end

  test "owned environments" do
    shop_menu = [
      {:spaniel, 34},
      {:ring_of_power, 75},
      {:rope, 4}
    ]

    RustlerTest.sublists(shop_menu)

    receive do
      x ->
        assert x == [
                 [],
                 [{:rope, 4}],
                 [{:ring_of_power, 75}],
                 [{:ring_of_power, 75}, {:rope, 4}],
                 [{:spaniel, 34}],
                 [{:spaniel, 34}, {:rope, 4}],
                 [{:spaniel, 34}, {:ring_of_power, 75}],
                 [{:spaniel, 34}, {:ring_of_power, 75}, {:rope, 4}]
               ]
    end
  end

  test "whereis_pid" do
    assert self() == RustlerTest.whereis_pid(self())

    {:ok, agent_pid} = Agent.start(fn -> 42 end, name: MyAgentName)
    assert agent_pid == RustlerTest.whereis_pid(MyAgentName)

    assert nil == RustlerTest.whereis_pid("not a PID")
    assert nil == RustlerTest.whereis_pid(:not_a_registered_name)
  end

  test "is_process_alive" do
    assert true == RustlerTest.is_process_alive(self())

    task =
      Task.async(fn ->
        receive do
          :exit -> :ok
        end
      end)

    assert true == RustlerTest.is_process_alive(task.pid)

    send(task.pid, :exit)
    Task.await(task)

    assert false == RustlerTest.is_process_alive(task.pid)
  end

  test "send_error" do
    task =
      Task.async(fn ->
        receive do
          :exit -> :ok
        end
      end)

    # A send to an alive process from an alive process should not return an
    # error
    assert :ok == RustlerTest.send(task.pid, :msg)
    assert :ok == RustlerTest.send(task.pid, :exit)
    Task.await(task)

    # Once the target process is down, sends should error
    assert :error == RustlerTest.send(task.pid, :msg)
    assert :error == RustlerTest.send(task.pid, :msg)
  end

  test "make_ref" do
    {different, ref1, ref2} = RustlerTest.make_refs()

    assert different
    assert is_reference(ref1)
    assert is_reference(ref2)
    assert ref1 != ref2
  end
end
