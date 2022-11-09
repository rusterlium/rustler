defmodule RustlerTest.DynamicDataTest do
  use ExUnit.Case, async: true

  test "rust has access to demo_dataset.txt via dynamic priv path" do
    %{priv_path: path} = DynamicData.Config.nif_data()
    path = Path.join(path, "demo_dataset.txt")

    assert File.read!(path) == DynamicData.get_dataset()
  end
end
