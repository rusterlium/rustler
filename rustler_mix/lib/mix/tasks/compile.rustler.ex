defmodule Mix.Tasks.Compile.Rustler do
  @moduledoc false

  use Mix.Task

  def run(_args) do
    IO.puts([
      IO.ANSI.yellow(),
      """

      The `:rustler` compiler has been deprecated since v0.22.0 and will be
      removed in v1.0.

      To remove this warning, please consult the CHANGELOG for v0.22.0.
      """,
      IO.ANSI.default_color()
    ])
  end
end
