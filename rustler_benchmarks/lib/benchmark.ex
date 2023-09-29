defmodule Benchmark do
  use Rustler, otp_app: :rustler_benchmarks, crate: "benchmark", mode: :release

  def nifstruct_benchmark(_input, _operation), do: error()
  def nifrecord_benchmark(_input, _operation), do: error()
  def encode_tagged_enum(), do: error()
  def decode_tagged_enum(_), do: error()
  def decode_struct(_), do: error()
  def decode_string(_), do: error()
  def decode_struct_string(_), do: error()
  def decode_term(_), do: error()
  def encode_atom(), do: error()
  def void(), do: error()
  def compare_atom(_), do: error()

  defp error do
    :erlang.nif_error(:nif_not_loaded)
  end
end
