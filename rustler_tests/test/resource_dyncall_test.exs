if RustlerTest.Helper.has_nif_version("2.16") do
  defmodule RustlerTest.ResourceDyncallTest do
    use ExUnit.Case, async: true

    test "perform dyncall" do
      pid = self()

      res = ResourceDyncall.new(pid)

      call_res = RustlerTest.perform_dyncall(res, 1, 2, 3)

      assert call_res == pid

      receive do
        {1, 2, 3} -> true
      after
        50 ->
          raise "fail"
      end
    end
  end
end
