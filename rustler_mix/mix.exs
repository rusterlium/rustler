defmodule Rustler.Mixfile do
  use Mix.Project

  @source_url "https://github.com/rusterlium/rustler"
  @version "0.39.0"

  def project do
    [
      app: :rustler,
      name: "Rustler",
      version: @version,
      elixir: "~> 1.18",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      docs: docs()
    ]
  end

  def application do
    [extra_applications: [:logger, :eex]]
  end

  defp elixirc_paths(env) when env in [:dev, :test], do: ["lib", "ex_doc"]
  defp elixirc_paths(_env), do: ["lib"]

  defp deps do
    [
      {:ex_doc, "~> 0.40.4", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.0", only: :dev, runtime: false},
      {:makeup_rust, ">= 0.0.0", only: [:dev, :test], runtime: false}
    ]
  end

  defp package do
    [
      description: "Mix compiler and runtime helpers for Rustler.",
      files: ["lib", "priv", "mix.exs", "README.md"],
      maintainers: ["hansihe"],
      licenses: ["MIT", "Apache-2.0"],
      links: %{
        "Changelog" => "https://hexdocs.pm/rustler/changelog.html",
        "GitHub" => @source_url
      }
    ]
  end

  defp docs do
    [
      retriever: Rustler.ExDoc.Retriever,
      extras: [
        "README.md",
        "../CHANGELOG.md",
        "../UPGRADE.md",
        {"../LICENSE-APACHE", [title: "License (Apache-2.0)"]},
        {"../LICENSE-MIT", [title: "License (MIT)"]}
      ],
      main: "readme",
      homepage_url: @source_url,
      source_url: @source_url,
      source_url_pattern: &source_url/2,
      formatters: [Rustler.ExDoc.HTML]
    ]
  end

  defp source_url(path, line) do
    path = path |> Path.expand() |> Path.relative_to(Path.expand("..", __DIR__))
    "#{@source_url}/blob/rustler-#{@version}/#{path}#L#{line}"
  end
end
