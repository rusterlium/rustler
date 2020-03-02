defmodule Rustler.Mixfile do
  use Mix.Project

  def project do
    [
      app: :rustler,
      version: rustler_version(),
      elixir: "~> 1.6",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      name: "Rustler Mix",
      source_url: "https://github.com/rustlerium/rustler",
      homepage_url: "https://github.com/rusterlium/rustler",
      deps: deps(),
      docs: [
        main: "readme",
        extras: ["README.md", "../CHANGELOG.md"],
        source_url_pattern:
          "https://github.com/rusterlium/rustler/blob/rustler-#{rustler_version()}/rustler_mix/%{path}#L%{line}"
      ],
      package: package(),
      description: description()
    ]
  end

  def rustler_version, do: "0.22.0-rc.0"

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
      {:toml, "~> 0.5.2", runtime: false},
      {:ex_doc, "~> 0.19", only: :dev}
    ]
  end

  defp description do
    """
    Mix compiler and runtime helpers for Rustler.
    """
  end

  defp package do
    [
      files: ["lib", "priv", "mix.exs", "README.md"],
      maintainers: ["hansihe"],
      licenses: ["MIT", "Apache-2.0"],
      links: %{"GitHub" => "https://github.com/rusterlium/rustler"}
    ]
  end
end
