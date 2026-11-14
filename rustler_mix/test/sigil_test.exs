defmodule Rustler.SigilTest do
  use ExUnit.Case, async: false

  test "writes collected CARGO and RUST sigils into a crate" do
    crate_path =
      Path.join(
        System.tmp_dir!(),
        "rustler-sigil-#{System.unique_integer([:positive])}"
      )

    on_exit(fn -> File.rm_rf!(crate_path) end)

    module = "Rustler.SigilGenerated#{System.unique_integer([:positive])}"

    Code.compile_string("""
    defmodule #{module} do
      use Rustler,
        otp_app: :rustler,
        path: #{inspect(crate_path)},
        skip_compilation?: true,
        lib: false

      ~CARGO\"""
      [package]
      name = "sigil_test"
      \"""

      ~CARGO\"""
      [lib]
      crate-type = ["cdylib"]
      \"""

      ~RUST\"""
      fn first() {}
      \"""

      ~RUST\"""
      fn second() {}
      \"""
    end
    """)

    cargo = File.read!(Path.join(crate_path, "Cargo.toml"))
    rust = File.read!(Path.join([crate_path, "src", "lib.rs"]))

    assert cargo =~ "[package]"
    assert cargo =~ ~s(name = "sigil_test")
    assert cargo =~ "[lib]"
    assert :binary.match(rust, "fn first()") < :binary.match(rust, "fn second()")
  end
end
