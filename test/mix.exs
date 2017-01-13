defmodule RustlerTest.Mixfile do
  use Mix.Project

  def project do
    [app: :rustler_test,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     compilers: [:rustler] ++ Mix.compilers,
     rustler_crates: rustler_crates(),
     deps: deps()]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:rustler, path: "../rustler_mix"}]
  end

  defp rust_nightly? do
    {version, 0} = System.cmd("rustc", ["--version"])
    version |> String.contains?("nightly")
  end

  defp rustler_crates do
    [rustler_test: [
        path: "/",
        mode: :debug,
        default_flags: !rust_nightly?(),
    ]]
  end
end
