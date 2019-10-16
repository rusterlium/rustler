defmodule DeprecatedMacros do
  use Rustler,
    otp_app: :rustler_test,
    crate: :deprecated_macros

  def add(_, _), do: :erlang.nif_error(:deprecated_macros_not_loaded)
end
