defmodule SerdeRustlerTests.Json.JsonTest do
  use ExUnit.Case, async: true

  alias SerdeRustlerTests.Json

  setup :read_data

  describe "Blockchain" do
    @describetag filename: "blockchain.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "Github" do
    @describetag filename: "github.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "Giphy" do
    @describetag filename: "giphy.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "GovTrack" do
    @describetag filename: "govtrack.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "Issue 90" do
    @describetag filename: "issue-90.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "JSON Generator (Pretty)" do
    @describetag filename: "json-generator-pretty.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "JSON Generator" do
    @describetag filename: "json-generator.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "Pokedex" do
    @describetag filename: "pokedex.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "UTF-8 escaped" do
    @describetag filename: "utf-8-escaped.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  describe "UTF-8 unescaped" do
    @describetag filename: "utf-8-unescaped.json"

    test "decode", ctx do
      run_decode(ctx)
    end

    test "encode", ctx do
      run_encode(ctx)
    end
  end

  defp read_data(ctx) do
    path = Path.expand("data/" <> ctx[:filename], __DIR__)
    file = File.read!(path)
    decoded = Jason.decode!(file)

    %{:jsonfile => file, :json => decoded}
  end

  defp run_decode(ctx) do
    expected = ctx[:json]
    actual = Json.decode(ctx[:jsonfile])

    assert expected == actual, ~s"""
      DECODING ERROR :: #{ctx[:filename]}
      expected: #{inspect(expected)}
      actual: #{inspect(actual)}
    """
  end

  defp run_encode(ctx) do
    expected = ctx[:json]
    actual = Json.encode(expected) |> Jason.decode!()

    assert expected == actual, ~s"""
      ENCODING ERROR :: #{ctx[:filename]}
      expected: #{inspect(expected)}
      actual: #{inspect(actual)}
    """
  end
end
