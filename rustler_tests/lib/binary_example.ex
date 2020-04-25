defmodule BinaryExample do
  use Rustler,
    otp_app: :rustler_test,
    crate: :binary_example,
    lib: false
end
