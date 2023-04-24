defmodule Rustler do
  @moduledoc """
  Provides compile-time configuration for a NIF module.

  When used, Rustler expects the `:otp_app` as option.
  The `:otp_app` should point to the OTP application that
  the dynamic library can be loaded from.

  For example:

      defmodule MyNIF do
        use Rustler, otp_app: :my_nif
      end

  This allows the module to be configured like so:

      config :my_nif, MyNIF,
        crate: :my_nif,
        load_data: [1, 2, 3]

  ## Configuration options

    * `:cargo` - Specify how to envoke the rust compiler. Options are:
        - `:system` (default) - use `cargo` from the system (must be in `$PATH`)
        - `{:system, <channel>}` - use `cargo` from the system with the given channel.
          Specified as a string, passed directly to `cargo` (e.g. "+nightly").
        - `{:rustup, <version>}` - use `rustup` to specify which channel to use.
          Available options include: `:stable`, `:beta`, `:nightly`, or a string
          which specifies a specific version (e.g. `"1.39.0"`).
        - `{:bin, "/path/to/binary"}` - provide a specific path to `cargo`.

    * `:crate` - the name of the Rust crate, if different from your `otp_app`
      value. If you have more than one crate in your project, you will need to
      be explicit about which crate you intend to use.

    * `:default_features` - a boolean to specify whether the crate's default features
      should be used.

    * `:env` - Specify a list of environment variables when envoking the compiler.

    * `:features` - a list of features to enable when compiling the crate.

    * `:load_data` - Any valid term. This value is passed into the NIF when it is
      loaded (default: `0`)

    * `:load_data_fun` - `{Module, :function}` to dynamically generate `load_data`.
      Default value: `nil`.

      This parameter is mutually exclusive with `load_data`
      which means that `load_data` has to be set to it's default value.

      Example

        defmodule NIF do
          use Rustler, load_data_fun: {Deployment, :nif_data}
        end

        defmodule Deployment do
          def nif_data do
            :code.priv_dir(:otp_app) |> IO.iodata_to_binary()
          end
        end

    * `:load_from` - This option allows control over where the final artifact should be
      loaded from at runtime. By default the compiled artifact is loaded from the
      owning `:otp_app`'s `priv/native` directory. This option comes in handy in
      combination with the `:skip_compilation?` option in order to load pre-compiled
      artifacts. To override the default behaviour specify a tuple:
      `{:my_app, "priv/native/<artifact>"}`. Due to the way `:erlang.load_nif/2`
      works, the artifact should not include the file extension (i.e. `.so`, `.dll`).

    * `:mode` - Specify which mode to compile the crate with (default: `:release`)

    * `:path` - By default, rustler expects the crate to be found in `native/<crate>` in the
      root of the project. Use this option to override this.

    * `:skip_compilation?` - This option skips envoking the rust compiler. Specify this option
      in combination with `:load_from` to load a pre-compiled artifact.

    * `:target` - Specify a compile [target] triple.

    * `:target_dir`: Override the compiler output directory.

  Any of the above options can be passed directly into the `use` macro like so:

      defmodule MyNIF do
        use Rustler,
          otp_app: :my_nif,
          crate: :some_other_crate,
          load_data: :something
      end

  [target]: https://doc.rust-lang.org/stable/rustc/platform-support.html
  """

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      config = Rustler.Compiler.compile_crate(__MODULE__, opts)

      for resource <- config.external_resources do
        @external_resource resource
      end

      if config.lib do
        @load_from config.load_from
        @load_data config.load_data
        @load_data_fun config.load_data_fun

        @before_compile Rustler
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      @on_load :rustler_init

      @doc false
      def rustler_init do
        # Remove any old modules that may be loaded so we don't get
        # {:error, {:upgrade, 'Upgrade not supported by this NIF library.'}}
        :code.purge(__MODULE__)

        {otp_app, path} = @load_from

        load_path =
          otp_app
          |> Application.app_dir(path)
          |> to_charlist()

        load_data = Rustler.construct_load_data(@load_data, @load_data_fun)

        :erlang.load_nif(load_path, load_data)
      end
    end
  end

  def construct_load_data(load_data, load_data_fun) do
    default_load_data_value = %Rustler.Compiler.Config{}.load_data
    default_fun_value = %Rustler.Compiler.Config{}.load_data_fun

    case {load_data, load_data_fun} do
      {load_data, ^default_fun_value} ->
        load_data

      {^default_load_data_value, {module, function}}
      when is_atom(module) and is_atom(function) ->
        apply(module, function, [])

      {^default_load_data_value, provided_value} ->
        raise """
        `load_data` has to be `{Module, :function}`.
        Instead received: #{inspect(provided_value)}
        """

      {load_data, load_data_fun} ->
        raise """
        Only `load_data` or `load_data_fun` can be provided. Instead received:
        >>> load_data: #{inspect(load_data)}
        >>> load_data_fun: #{inspect(load_data_fun)}
        """
    end
  end

  @doc false
  def rustler_version, do: "0.28.0"

  @doc """
  Supported NIF API versions.
  """
  def nif_versions,
    do: [
      '2.14',
      '2.15',
      '2.16'
    ]
end
