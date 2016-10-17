defmodule Rustler.Mixfile do
  use Mix.Project

  def project do
    [app: :rustler,
     version: "0.5.0",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     name: "Rustler Mix",
     source_url: "https://github.com/hansihe/Rustler",
     homepage_url: "https://github.com/hansihe/Rustler",
     deps: deps,
     docs: [
       extras: ["guides/Basics.md"],
       source_url_pattern: "https://github.com/hansihe/Rustler/blob/master/rustler_mix/%{path}#L%{line}"
     ],
     package: package,
     description: description]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [{:ex_doc, "~> 0.12", only: :dev}]
  end

  defp description do
    """
    Mix compiler and runtime helpers for Rustler.
    """
  end

  defp package do
    [
      files: ["lib", "src", "priv", "mix.exs", "README.md"],
      maintainers: ["hansihe"],
      licenses: ["MIT", "Apache-2.0"],
      links: %{"GitHub" => "https://github.com/hansihe/Rustler"},
    ]
  end
end
