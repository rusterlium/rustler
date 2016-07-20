defmodule Rustler do

  def rustler_version, do: "0.9.1"
  def rust_version, do: "nightly-2016-07-19"
  def nif_versions, do: [
    '2.7',
    '2.8',
    '2.9',
    '2.10',
    '2.11',
  ]

  def nif_lib_dir do
    Mix.Project.build_path <> "/rustler"
  end

  def nif_lib_path(lib_name) do
    "#{nif_lib_dir}/lib#{lib_name}"
  end

  defmacro load_nif(lib_name, load_args \\ nil) do
    quote do
      :erlang.load_nif(Rustler.nif_lib_path(unquote(lib_name)), unquote(load_args))
    end
  end

end
