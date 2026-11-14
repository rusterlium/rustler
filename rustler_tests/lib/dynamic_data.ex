defmodule DynamicData.Config do
  def nif_data do
    %{priv_path: :code.priv_dir(:rustler_test) |> IO.iodata_to_binary()}
  end
end

defmodule DynamicData do
  use Rustler,
    otp_app: :rustler_test,
    crate: :dynamic_load,
    load_data_fun: {DynamicData.Config, :nif_data}
end
