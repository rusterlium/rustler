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
            binaries: [],
            load_data: 0,
            load_data_fun: nil,
            load_from: nil,
            mode: :release,
            otp_app: nil,
            path: "",
            skip_compilation?: false,
            target: nil,
            metadata: nil

  alias Rustler.Compiler.Config

  def from(otp_app, config, opts) do
    crate = config[:crate] || opts[:crate] || otp_app

    defaults = %Config{
      crate: crate,
      mode: :release,
      otp_app: otp_app,
      path: "native/#{crate}",
      load_from: {otp_app, "priv/native/#{crate}"}
    }

    defaults
    |> Map.from_struct()
    |> Enum.into([])
    |> Keyword.merge(opts)
    |> Keyword.merge(config)
    |> build()
  end

  defp build(opts) do
    opts =
      if opts[:skip_compilation?] do
        opts
      else
        crate = Keyword.fetch!(opts, :crate)
        crate_path = Keyword.fetch!(opts, :path)

        metadata = metadata!(crate_path)
        resources = external_resources(crate_path, crate, metadata)

        opts
        |> Keyword.put(:metadata, metadata)
        |> Keyword.put(:external_resources, resources)
      end

    struct!(Config, opts)
  end

  defp metadata!(crate_path) do
    metadata =
      case System.cmd("cargo", ~w(metadata --format-version=1), cd: crate_path) do
        {metadata, 0} ->
          metadata

        {output, _code} ->
          raise "calling `cargo metadata` failed.\n" <> output
      end

    JSON.decode!(metadata)
  end

  defp external_resources(crate_path, crate, metadata) do
    crate_str = to_string(crate)
    packages = Map.fetch!(metadata, "packages")
    crate_spec = get_spec(packages, crate_str)

    packages
    |> gather_local_crates([crate_spec], [crate_path], MapSet.new([crate_str]))
    |> Enum.flat_map(&expand_paths/1)
  end

  # The walk never enters the Cargo target. A target can hold a huge tree, and
  # the walk follows symlinks, so two links back to their own directory there
  # would make it endless.
  defp expand_paths(path) do
    path
    |> list_dir()
    |> List.delete("target")
    |> Enum.flat_map(&files(Path.join(path, &1)))
  end

  defp files(path) do
    if File.dir?(path) do
      path |> list_dir() |> Enum.flat_map(&files(Path.join(path, &1)))
    else
      [path]
    end
  end

  # Leaves out the names that start with a dot, as `Path.wildcard/1` does.
  defp list_dir(path) do
    case File.ls(path) do
      {:ok, names} -> names |> Enum.reject(&String.starts_with?(&1, ".")) |> Enum.sort()
      {:error, _} -> []
    end
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

    as_specs =
      local_deps
      |> Enum.map(&get_spec(packages, &1["name"]))
      |> Enum.reject(&is_nil/1)

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
end
