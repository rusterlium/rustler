defmodule Rustler do

  def rustler_version, do: "0.12.0"
  def nif_versions, do: [
    '2.7',
    '2.8',
    '2.9',
    '2.10',
    '2.11',
  ]

  def nif_lib_dir(application) do
    to_string(:code.priv_dir(application)) <> "/rustler"
  end

  def nif_lib_path(application, lib_name) do
    "#{nif_lib_dir(application)}/lib#{lib_name}"
  end

  defmacro load_nif(app_name, lib_name, load_args \\ nil) do
    quote do
      :erlang.load_nif(Rustler.nif_lib_path(unquote(app_name), unquote(lib_name)), unquote(load_args))
    end
  end

end
