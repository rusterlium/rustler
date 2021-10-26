defmodule Rustler.Compiler.Config do
  @moduledoc false

  @type rust_version :: :stable | :beta | :nightly | binary()
  @type cargo :: :system | {:rustup, rust_version()} | {:bin, Path.t()}
  @type crate :: binary()
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

    # TODO: Remove in 1.0
    rustler_crates = Mix.Project.config()[:rustler_crates] || []
    legacy_config = rustler_crates[to_atom(crate)] || []

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
    |> Keyword.merge(legacy_config)
    |> Keyword.merge(opts)
    |> Keyword.merge(config)
    |> build()
  end

  defp build(opts) do
    crate = Keyword.fetch!(opts, :crate)

    resources =
      if opts[:skip_compilation?] do
        []
      else
        opts
        |> Keyword.get(:path)
        |> external_resources(crate)
      end

    opts = Keyword.put(opts, :external_resources, resources)

    struct!(Config, opts)
  end

  defp external_resources(crate_path, crate) do
    crate_str = to_string(crate)

    metadata =
      case System.cmd("cargo", ~w(metadata --format-version=1), cd: crate_path) do
        {metadata, 0} ->
          metadata

        {output, code} ->
          raise "calling `cargo metadata` failed.\n" <> output
      end

    json = Jason.decode!(metadata)

    packages = json["packages"]

    crate_spec = get_spec(packages, crate_str)

    packages
    |> gather_local_crates([crate_spec], [crate_path], MapSet.new([crate_str]))
    |> Enum.flat_map(&expand_paths/1)
  end

  defp expand_paths(path) do
    path
    |> Path.join("**/*")
    |> Path.wildcard()
    |> Enum.reject(&String.starts_with?(&1, "#{path}/target/"))
  end

  defp local_crate?(package) do
    package["path"]
  end

  defp gather_local_crates(_, [], paths, _visited) do
    paths
  end

  defp gather_local_crates(packages, [current_spec | rest], paths_acc, visited) do
    local_deps =
      current_spec["dependencies"]
      |> Enum.filter(&local_crate?/1)
      |> Enum.reject(fn dep -> MapSet.member?(visited, dep["name"]) end)

    paths = Enum.map(local_deps, & &1["path"]) ++ paths_acc

    as_specs = Enum.map(local_deps, &get_spec(packages, &1["name"]))

    visited =
      local_deps
      |> MapSet.new(& &1["name"])
      |> MapSet.union(visited)

    gather_local_crates(packages, as_specs ++ rest, paths, visited)
  end

  defp get_spec(packages, name) do
    packages
    |> Enum.filter(&(&1["name"] == name))
    |> List.first()
  end

  defp build_mode(env) when env in [:prod, :bench], do: :release
  defp build_mode(_), do: :debug

  defp to_atom(name) when is_binary(name),
    do: String.to_atom(name)

  defp to_atom(name) when is_atom(name),
    do: name
end
