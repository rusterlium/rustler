defmodule Rustler.ConfigTest do
  use ExUnit.Case, async: true
  alias Rustler.Config

  test "from/2 with default opts" do
    assert %Config{
             cargo: :system,
             crate: "rustler",
             default_features: true,
             env: [],
             external_resources: [],
             features: [],
             lib: true,
             load_data: 0,
             load_from: {:rustler, "priv/native/librustler"},
             mode: :debug,
             module: Foo.Bar,
             otp_app: :rustler,
             path: "native/rustler",
             priv_dir: "",
             skip_compilation?: true,
             target: nil,
             target_dir: target_dir
           } = Config.from(:rustler, Foo.Bar, skip_compilation?: true)

    assert String.ends_with?(target_dir, "_build/test/lib/rustler/native/rustler")
  end

  test "from/2 with precompiled opts" do
    assert %Config{
             cargo: :system,
             crate: "rustler",
             default_features: true,
             env: [],
             external_resources: [],
             features: [],
             lib: true,
             load_data: 0,
             load_from: {:rustler, "priv/native/librustler"},
             mode: :debug,
             module: Foo.Bar,
             otp_app: :rustler,
             path: "native/rustler",
             priv_dir: "",
             skip_compilation?: true,
             target: nil,
             target_dir: _target_dir
           } = Config.from(:rustler, Foo.Bar, precompiled: [base_url: "foo", version: "bar"])
  end
end
