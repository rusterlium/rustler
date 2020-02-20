defmodule Rustler.Compiler.Config do
  @moduledoc false

  @type rust_version :: :stable | :beta | :nightly | binary()
  @type cargo :: :system | {:rustup, rust_version()} | {:bin, Path.t()}
  @type crate :: atom()
  @type default_features :: boolean()
  @type env :: [{binary(), binary()}]
  @type features :: [binary()]
  @type mode :: :debug | :release
  @type load_data :: term()
  @type path :: Path.t()

  defstruct cargo: :system,
            crate: nil,
            default_features: true,
            env: [],
            external_resources: [],
            features: [],
            lib: true,
            load_data: 0,
            load_from: nil,
            mode: :release,
            otp_app: nil,
            path: "",
            priv_dir: "",
            skip_compilation?: false,
            target: nil,
            target_dir: ""

  alias Rustler.Compiler.Config

  def from(otp_app, module, opts) do
    config = Application.get_env(otp_app, module, [])

    crate = config[:crate] || opts[:crate] || otp_app

    defaults = %Config{
      crate: crate,
      load_from: {otp_app, "priv/native/lib#{crate}"},
      mode: build_mode(Mix.env()),
      otp_app: otp_app,
      path: "native/#{crate}",
      target_dir: Application.app_dir(otp_app, "native/#{crate}")
    }

    defaults
    |> Map.from_struct()
    |> Enum.into([])
    |> Keyword.merge(opts)
    |> Keyword.merge(config)
    |> build()
  end

  defp build(opts) do
    resources =
      opts
      |> Keyword.get(:path)
      |> external_resources()

    opts = Keyword.put(opts, :external_resources, resources)

    struct!(Config, opts)
  end

  defp external_resources(crate_path) do
    "#{crate_path}/**/*"
    |> Path.wildcard()
    |> Enum.reject(fn path ->
      String.starts_with?(path, "#{crate_path}/target/")
    end)
  end

  defp build_mode(env) when env in [:prod, :bench], do: :release
  defp build_mode(_), do: :debug
end
