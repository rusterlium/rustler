defmodule RustlerBigintTest do
  use Rustler,
    otp_app: :rustler_test,
    crate: :rustler_bigint_test

  defp err do
    throw(NifNotLoadedError)
  end

  def echo(_), do: err()
  def add_one(_), do: err()
  def add(_, _), do: err()
end
