if RustlerTest.Helper.has_nif_version("2.16") do
  defmodule ResourceDyncall do
    use Rustler,
      otp_app: :rustler_test,
      crate: :resource_dyncall

    def new(_), do: err()

    defp err(), do: :erlang.nif_error(:nif_not_loaded)
  end
end
