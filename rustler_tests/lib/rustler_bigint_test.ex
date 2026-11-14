defmodule RustlerBigintTest do
  use Rustler,
    otp_app: :rustler_test,
    crate: :rustler_bigint_test
end
