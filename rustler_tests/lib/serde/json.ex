defmodule SerdeRustlerTests.Json do
  @doc "Decodes the JSON string into an Elixir term."
  def decode(file), do: SerdeRustlerTests.decode_json(file)

  @doc "Encodes the Elixir term into a compact JSON string."
  def encode(term), do: SerdeRustlerTests.encode_json_pretty(term)
end
