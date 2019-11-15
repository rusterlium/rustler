defmodule Rustler do
  @moduledoc """
  Provides compile-time configuration for a NIF module.

  When used, Rustler expects the `:otp_app` as option.
  The `:otp_app` should point to the OTP application that
  the dynamic library can be loaded from. For example:

      defmodule MyNIF do
        use Rustler, otp_app: :my_nif
      end

  This allows the module to be configured like so:

      config :my_nif, MyNIF,
        crate: :my_nif,
        load_data: [1, 2, 3]

  Configuration options:

    * `:crate` - the name of the Rust crate (as an atom), if different from your
      `otp_app` value. If you have more than one crate in your project, you will
      need to be explicit about which crate you intend to use.

    * `:load_data` - Any valid term. This value is passed into the NIF when it is
      loaded (default: `0`)

  Either of the above options can be passed directly into the `use` macro like so:

      defmodule MyNIF do
        use Rustler,
          otp_app: :my_nif,
          crate: :some_other_crate,
          load_data: :something
      end

  """

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      config = Rustler.Compiler.compile_crate(__MODULE__, opts)

      for resource <- config.external_resources do
        @external_resource resource
      end

      @load_from config.load_from
      @load_data config.load_data

      @before_compile Rustler
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      @on_load :rustler_init

      @doc false
      def rustler_init do
        # Remove any old modules that may be loaded so we don't get
        # :error, {:upgrade, 'Upgrade not supported by this NIF library.'}}
        :code.purge(__MODULE__)

        {otp_app, path} = @load_from

        load_path =
          otp_app
          |> Application.app_dir(path)
          |> to_charlist()

        :erlang.load_nif(load_path, @load_data)
      end
    end
  end

  @doc false
  def rustler_version, do: "0.21.0"

  @doc """
  Supported NIF API versions.
  """
  def nif_versions,
    do: [
      '2.7',
      '2.8',
      '2.9',
      '2.10',
      '2.11',
      '2.12',
      '2.13',
      '2.14',
      '2.15'
    ]
end
