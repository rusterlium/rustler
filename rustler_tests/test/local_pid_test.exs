defmodule RustlerTest.LocalPidTest do
  use ExUnit.Case, async: true

  def make_pid() do
    {:ok, pid} = Task.start(fn -> :ok end)
    pid
  end

  def compare(lhs, rhs) do
    cond do
      lhs < rhs -> -1
      lhs == rhs -> 0
      lhs > rhs -> 1
    end
  end

  test "local pid comparison" do
    # We make sure that the code we have in rust code matches the comparisons
    # that are performed in the BEAM code.
    pids = for _ <- 1..3, do: make_pid()

    for lhs <- pids, rhs <- pids do
      assert RustlerTest.compare_local_pids(lhs, rhs) == compare(lhs, rhs)
    end
  end

  test "local pid equality" do
    pids = for _ <- 1..3, do: make_pid()

    for lhs <- pids, rhs <- pids do
      expected = lhs == rhs
      assert RustlerTest.are_equal_local_pids(lhs, rhs) == expected
    end
  end
end
