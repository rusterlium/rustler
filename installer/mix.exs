defmodule Installer.Mixfile do
  use Mix.Project

  def project do
    [app: :rustler_installer,
     version: "0.0.1",
     elixir: "~> 1.1"]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:logger]]
  end
end
