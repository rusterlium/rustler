defmodule Rustler.CargoTestRunner do
  @moduledoc """
  Run `cargo test`.
  """

  use Rustler, otp_app: :rustler, crate: "rustler_cargo_test_runner"

  @doc false
  def run_tests(_cwd, _homedir, _manifest_path), do: :erlang.nif_error(:not_loaded)
end
