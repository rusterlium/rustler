defmodule RustlerBenchmarks.MixProject do
  use Mix.Project

  def project do
    [
      app: :rustler_benchmarks,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.0"},
      {:rustler, path: "../rustler_mix", runtime: false}
    ]
  end
end
