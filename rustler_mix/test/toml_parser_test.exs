defmodule Rustler.TomlParserTest do
  use ExUnit.Case

  @simple_toml """
  root_var = 5

  [package]
  name = "rustler"
  description = "Safe Rust wrappers for creating Erlang NIF functions"
  repository = "https://github.com/hansihe/Rustler"
  version = "0.8.1"
  authors = ["Hansihe <me@hansihe.com>"]
  license = "MIT/Apache-2.0"

  [dependencies]
  ruster_unsafe = ">=0.2"
  libc = ">=0.1"
  lazy_static = "1.0"

  [[testing]]
  wooo = "hoo"
  """

  test "simple toml" do
    res = Rustler.TomlParser.parse(@simple_toml)
    assert Rustler.TomlParser.get_table_val(res, ["dependencies"], "libc") == ">=0.1"
    assert Rustler.TomlParser.get_table_val(res, ["dependencies"], "none") == nil
    assert Rustler.TomlParser.get_table_val(res, ["testing"], "wooo") == nil
    assert Rustler.TomlParser.get_table_val(res, [], "root_var") == 5
  end
end
