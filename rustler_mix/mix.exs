defmodule Rustler.Mixfile do
  use Mix.Project

  @source_url "https://github.com/rusterlium/rustler"
  @version "0.37.1"

  def project do
    [
      app: :rustler,
      name: "Rustler",
      version: @version,
      elixir: "~> 1.15",
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

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:credo, "~> 1.0", only: :dev, runtime: false},
      {:makeup_rust, ">= 0.0.0", only: :dev, runtime: false},
      {:jason, "~> 1.0", runtime: false}
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
        "README.md",
        "../CHANGELOG.md",
        "../UPGRADE.md",
        {"../LICENSE-APACHE", [title: "License (Apache-2.0)"]},
        {"../LICENSE-MIT", [title: "License (MIT)"]},
        {"../doc/rust-api.md", [title: "Rust API"]},
        {"../doc/rust-codegen.md", [title: "Rust Code Generation"]},
      ],
      main: "readme",
      homepage_url: @source_url,
      source_url: @source_url,
      source_url_pattern: "#{@source_url}/blob/rustler-#{@version}/rustler_mix/%{path}#L%{line}",
      formatters: ["html"]
    ]
  end
end
