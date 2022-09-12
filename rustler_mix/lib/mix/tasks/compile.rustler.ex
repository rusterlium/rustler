defmodule Mix.Tasks.Compile.Rustler do
  @moduledoc false

  use Mix.Task

  def run(_args) do
    cache_mix_env()
  end

  defp cache_mix_env() do
    File.mkdir(cache_target_path())
    mix_cache_path = mix_cache_path()
    current_mix_env = inspect(Mix.env())

    case File.read(mix_cache_path) do
      {:ok, ^current_mix_env} ->
        :ok

      _ ->
        File.write!(mix_cache_path, current_mix_env)
    end
  end

  defp cache_target_path do
    # We need to cache directly under `_build`, as the cache needs to be
    # overwritten if the build environment changes
    Mix.Project.build_path()
    |> Path.join("..")
    |> Path.join("rustler_cache")
  end

  def mix_cache_path do
    Path.join(cache_target_path(), "mix_env.cache")
  end
end
