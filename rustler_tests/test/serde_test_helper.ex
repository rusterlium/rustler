defmodule SerdeRustlerTests.Helpers do
  @moduledoc false
  use ExUnit.Case

  @doc """
  Runs the "serialize" test in the Rust NIF library
  """
  def run_ser(test_name, expected_term) do
    serialized = SerdeRustlerTests.test("serialize", test_name, expected_term)

    err_message = ~s"""
      SERIALIZATION ERROR :: #{test_name}
      expected: #{inspect(expected_term)}
      actual: #{inspect_serde(serialized)}
    """

    assert serialized == :ok, err_message
  end

  @doc """
  Runs the "deserialize" test in the Rust NIF library
  """
  def run_de(test_name, expected_term) do
    deserialized = SerdeRustlerTests.test("deserialize", test_name, expected_term)

    err_message = ~s"""
      DESERIALIZATION ERROR :: #{test_name}
      term: #{inspect(expected_term)}
      rust: #{inspect_serde(deserialized)}
    """

    assert deserialized == :ok, err_message
  end

  @doc "Runs the \"transcode\" test in the Rust NIF library, comparing the transcoded `expected_term` to itself."
  def run_transcode(test_name, expected_term) do
    run_transcode(test_name, expected_term, expected_term)
  end

  @doc "Runs the \"transcode\" test in the Rust NIF library, comparing the transcoded `in_term` to `out_term`"
  def run_transcode(test_name, in_term, out_term) do
    transcoded = SerdeRustlerTests.transcode(in_term)

    err_message = ~s"""
      TRANSCODING ERROR :: #{test_name}
      expected: #{inspect(out_term)}
      #{inspect_transcode(transcoded)}
    """

    assert transcoded == {:ok, out_term}, err_message
  end

  @doc "Adds a `:skip` flag to the test context"
  def skip(ctx, test_type), do: Map.put(ctx, :skip, test_type)

  @doc "Quickly extracts a float from a binary"
  def to_float(<<float::big-signed-float-size(32)>>), do: float
  def to_float(<<float::big-signed-float-size(64)>>), do: float

  defp inspect_serde(:ok), do: ""
  defp inspect_serde({:ok, _}), do: ""
  defp inspect_serde({:error, term}), do: inspect(term)

  defp inspect_transcode({:ok, term}), do: "actual: #{inspect(term)}"
  defp inspect_transcode({:error, reason}), do: "received error: #{inspect(reason)}"
end
