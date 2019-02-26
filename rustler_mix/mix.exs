defmodule Rustler.Mixfile do
  use Mix.Project

  def project do
    [app: :rustler,
     version: "0.20.0", # rustler_mix version
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     name: "Rustler Mix",
     source_url: "https://github.com/hansihe/Rustler",
     homepage_url: "https://github.com/hansihe/Rustler",
     deps: deps(),
     docs: [
       extras: ["guides/Basics.md"],
       source_url_pattern: "https://github.com/hansihe/Rustler/blob/master/rustler_mix/%{path}#L%{line}"
     ],
     package: package(),
     description: description()]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:ex_doc, "~> 0.19", only: :dev}]
  end

  defp description do
    """
    Mix compiler and runtime helpers for Rustler.
    """
  end

  defp package do
    [files: ["lib", "src", "priv", "mix.exs", "README.md"],
    exclude_patterns: ["src/toml_lexer.erl", "src/toml_parser.erl"],
     maintainers: ["hansihe"],
     licenses: ["MIT", "Apache-2.0"],
     links: %{"GitHub" => "https://github.com/hansihe/Rustler"}]
  end
end
