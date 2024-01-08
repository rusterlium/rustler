defmodule SerdeRustlerTests do
  @moduledoc """
  NIF wrapping Serializer and Deserializer tests written in the `native` rust
  crate.
  """

  use Rustler,
    otp_app: :rustler_test,
    crate: :rustler_serde_test

  def decode_json(_json_string), do: err()
  def decode_json_dirty(_json_string), do: err()
  def encode_json_compact(_term), do: err()
  def encode_json_compact_dirty(_term), do: err()
  def encode_json_pretty(_term), do: err()
  def encode_json_pretty_dirty(_term), do: err()
  def readme(_animal), do: err()
  def test(_test_impl, _test_name, _expected_term), do: err()
  def transcode(_term), do: err()
  def transcode_dirty(_term), do: err()

  defp err(), do: :erlang.nif_error(:nif_not_loaded)
end
