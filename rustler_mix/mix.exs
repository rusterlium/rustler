defmodule Rustler.Mixfile do
  use Mix.Project

  @source_url "https://github.com/rusterlium/rustler"
  @version "0.23.0"

  def project do
    [
      app: :rustler,
      name: "Rustler",
      version: @version,
      elixir: "~> 1.11",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      docs: docs()
    ]
  end

  def application do
    [extra_applications: [:logger, :eex, :inets, :public_key]]
  end

  defp deps do
    [
      {:castore, "~> 0.1.13"},
      {:toml, "~> 0.5.2", runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:bypass, "~> 2.1", only: :test},
      {:jason, "~> 1.2", runtime: false}
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
      extras: [
        "../CHANGELOG.md",
        {:"../LICENSE-APACHE", [title: "License (Apache-2.0)"]},
        {:"../LICENSE-MIT", [title: "License (MIT)"]},
        "../PRECOMPILATION_GUIDE.md",
        "README.md"
      ],
      main: "readme",
      homepage_url: @source_url,
      source_url: @source_url,
      source_url_pattern: "#{@source_url}/blob/rustler-#{@version}/rustler_mix/%{path}#L%{line}",
      formatters: ["html"]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["test/support", "lib"]
  defp elixirc_paths(_), do: ["lib"]
end
