defmodule BinaryExampleTest do
  use ExUnit.Case

  test "binary is compiled" do
    assert File.exists?("priv/native/binary_example")
    assert File.exists?("priv/native/hello_rust")
    assert File.exists?("priv/native/hello_rust2")
  end
end
