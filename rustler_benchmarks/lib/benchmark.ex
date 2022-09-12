defmodule Benchmark do
  use Rustler, otp_app: :rustler_benchmarks, crate: "benchmark"

  def nifstruct_benchmark(_input, _operation), do: error()
  def nifrecord_benchmark(_input, _operation), do: error()

  defp error do
    :erlang.nif_error(:nif_not_loaded)
  end
end
