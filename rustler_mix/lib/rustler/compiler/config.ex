defmodule Rustler.Compiler.Config do
  @type rust_version :: :stable | :beta | :nightly | binary()
  @type cargo :: :system | {:rustup, rust_version()} | {:bin, Path.t()}
  @type crate :: atom()
  @type default_features :: [binary()]
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

    cargo = Keyword.get(config, :cargo, :system)
    crate = config[:crate] || opts[:crate] || otp_app
    default_features = Keyword.get(config, :default_features, true)
    env = Keyword.get(config, :env, [])
    features = Keyword.get(config, :features, [])
    mode = config[:mode] || opts[:mode] || build_mode(Mix.env())
    load_data = config[:load_data] || opts[:load_data] || 0
    load_from = {config[:load_from] || otp_app, "priv/native/lib#{crate}"}
    path = config[:path] || "native/#{crate}"
    target = config[:target]
    external_resources = external_resources(path)
    target_dir = Keyword.get(config, :target_dir, Application.app_dir(otp_app, "native/#{crate}"))
    skip_compilation? = Keyword.get(config, :skip_compilation?, false)

    %Config{
      cargo: cargo,
      crate: crate,
      default_features: default_features,
      env: env,
      external_resources: external_resources,
      features: features,
      load_data: load_data,
      load_from: load_from,
      mode: mode,
      otp_app: otp_app,
      path: path,
      skip_compilation?: skip_compilation?,
      target: target,
      target_dir: target_dir
    }
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
