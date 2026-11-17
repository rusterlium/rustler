defmodule Rustler.CoreTest do
  use ExUnit.Case, async: true

  test "signature paths strip dynamic library extensions only" do
    for extension <- [".so", ".dll", ".dylib"] do
      assert :rustler.signature_path("libfoo" <> extension) == "libfoo.signature-v1.txt"
    end

    assert :rustler.signature_path("libfoo.beam") == "libfoo.beam.signature-v1.txt"
  end
end
