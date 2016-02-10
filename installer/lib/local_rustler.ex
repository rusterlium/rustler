defmodule Mix.Tasks.Local.Rustler do
  use Mix.Task

  @url "https://github.com/hansihe/rustler_archives/raw/master/rustler_installer.ez"
  @shortdoc "Updates Rustler locally"

  @moduledoc """
  Updates Rustler locally.
  mix local.rustler

  Accepts the same command line options as `archive.install`.
  """

  def run(args) do
    Mix.Task.run "archive.install", [@url|args]
  end

end
