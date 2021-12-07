defmodule Mix.Tasks.Rustler.NewTest do
  use ExUnit.Case, async: true
  import MixHelper

  @tag :tmp_dir
  test "generating a basic project", %{tmp_dir: tmp_dir} do
    in_tmp(tmp_dir, fn ->
      Mix.Tasks.Rustler.New.run(~w(--module MyApp.NativeMath --name my_math --otp-app my_app))

      assert_received {:mix_shell, :info, ["* creating native/my_math/README.md" <> _]}
      assert_received {:mix_shell, :info, ["* creating native/my_math/Cargo.toml" <> _]}
      assert_received {:mix_shell, :info, ["* creating native/my_math/src/lib.rs" <> _]}
      assert_received {:mix_shell, :info, ["* creating native/my_math/.cargo/config" <> _]}

      assert_received {:mix_shell, :info, ["Ready to go! See" <> _]}

      expected = """
      #[rustler::nif]
      fn add(a: i64, b: i64) -> i64 {
          a + b
      }

      rustler::init!("Elixir.MyApp.NativeMath", [add]);
      """

      # Remove "\r" on Windows
      file_contents =
        "native/my_math/src/lib.rs"
        |> File.read!()
        |> String.replace("\r", "")

      assert file_contents == expected
    end)
  end
end
