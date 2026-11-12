defmodule Rustler.Compiler.ConfigTest do
  use ExUnit.Case, async: true

  alias Rustler.Compiler.Config

  @moduletag :tmp_dir

  # Two links back to their own directory make a tree without end. A walk of
  # the target that follows them never returns, so the timeout fails the test.
  @tag timeout: 10_000
  test "external resources list the crate files and never walk the Cargo target", %{tmp_dir: dir} do
    File.mkdir_p!(Path.join(dir, "src"))

    File.write!(Path.join(dir, "Cargo.toml"), """
    [package]
    name = "foo"
    version = "0.1.0"
    edition = "2021"

    [workspace]
    """)

    File.write!(Path.join(dir, "src/lib.rs"), "")
    File.mkdir_p!(Path.join(dir, "data[1]"))
    File.write!(Path.join(dir, "data[1]/input.bin"), "")

    lib = Path.join(dir, "target/release/build/foo-1/out/lib")
    File.mkdir_p!(lib)
    # Leave no endless tree behind for other tools that follow symlinks.
    on_exit(fn -> File.rm_rf!(lib) end)
    File.write!(Path.join(lib, "libfoo.so"), "")
    File.ln_s!(".", Path.join(lib, "intel64"))
    File.ln_s!(".", Path.join(lib, "em64t"))

    resources = Config.from(:foo, [], path: dir).external_resources

    assert Path.join(dir, "Cargo.toml") in resources
    assert Path.join(dir, "src/lib.rs") in resources
    assert Path.join(dir, "data[1]/input.bin") in resources
    refute Enum.any?(resources, &String.starts_with?(&1, Path.join(dir, "target")))
  end
end
