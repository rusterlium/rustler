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

    * `:load_from` - This option allows control over where the final artifact should be
      loaded from at runtime. By default the compiled artifact is loaded from the
      owning `:otp_app`'s `priv/native` directory. This option comes in handy in
      combination with the `:skip_compilation?` option in order to load pre-compiled
      artifacts. To override the default behaviour specify a tuple:
      `{:my_app, "priv/native/<artifact>"}`. Due to the way `:erlang.load_nif/2`
      works, the artifact should not include the file extension (i.e. `.so`, `.dll`).
      In case you want to load precompiled NIFs from an external source (eg. GitHub releases),
      you can use the `:precompiled` option.

    * `:mode` - Specify which mode to compile the crate with. If you do not specify
      this option, a default will be provide based on the `Mix.env()`:
      - When `Mix.env()` is `:dev` or `:test`, the crate will be compiled in `:debug` mode.
      - When `Mix.env()` is `:prod` or `:bench`, the crate will be compiled in `:release` mode.

    * `:path` - By default, rustler expects the crate to be found in `native/<crate>` in the
      root of the project. Use this option to override this.

    * `:precompiled` - This option does something similar to `:load_from`, but it will load
      a precompiled NIF from an external source. This will require an especial attention to
      the release flow and security. You can check more details in the [precompilation guide].
      It is a keyword list and requires the following fields:
      - `:base_url` - the URL where the precompiled artifacts are published.
      - `:version` - the version of the given precompiled artifact.

    * `:skip_compilation?` - This option skips envoking the rust compiler. Specify this option
      in combination with `:load_from` to load a pre-compiled artifact. If `:precompiled` option
      is given, you can set `:skip_compilation?` to `false` in order to force the compilation
      for a given environment.

    * `:target` - Specify a compile [target] triple.

    * `:target_dir`: Override the compiler output directory.

  Any of the above options can be passed directly into the `use` macro like so:

      defmodule MyNIF do
        use Rustler,
          otp_app: :my_nif,
          crate: :some_other_crate,
          load_data: :something
      end

  [target]: https://forge.rust-lang.org/release/platform-support.html
  [precompilation guide]: https://github.com/rusterlium/rustler/blob/master/PRECOMPILATION_GUIDE.md 
  """

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      # TODO: should we use "Mix.shell()" like the compiler?
      require Logger

      otp_app = Keyword.fetch!(opts, :otp_app)
      config = Rustler.Config.from(otp_app, __MODULE__, opts)

      env_var_name = "#{String.upcase(to_string(config.crate))}_NIF_BUILD"

      config =
        cond do
          System.get_env(env_var_name) in ["1", "true"] ->
            Rustler.Compiler.compile_crate(config)

          Rustler.Config.use_precompilation?(config) ->
            case Rustler.Precompiled.download_or_reuse_nif_file(config) do
              {:ok, new_config} ->
                new_config

              {:error, precomp_error} ->
                error =
                  "Error while downloading precompiled NIF: #{precomp_error}\n\n" <>
                    "Set #{env_var_name}=1 env var to compile the NIF from scratch. " <>
                    "You can also configure this application to force compilation:\n\n" <>
                    "    config #{inspect(config.otp_app)}, #{inspect(__MODULE__)}, skip_compilation?: false\n"

                if Mix.env() == :prod do
                  raise error
                else
                  Logger.debug(error)
                  Rustler.Compiler.compile_crate(config)
                end
            end

          config.skip_compilation? ->
            config

          true ->
            Rustler.Compiler.compile_crate(config)
        end

      for resource <- config.external_resources do
        @external_resource resource
      end

      if config.lib do
        @load_from config.load_from
        @load_data config.load_data

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

        :erlang.load_nif(load_path, @load_data)
      end
    end
  end

  @doc false
  def rustler_version, do: "0.23.0"

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
