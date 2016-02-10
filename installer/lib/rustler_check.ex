defmodule Mix.Tasks.Rustler.Check do
  use Mix.Task

  @shortdoc "Checks that the system has everything needed to build a Rustler project"

  @supported_nif_versions ["2.7", "2.8", "2.9"]

  defmodule RustCVersion do
    defstruct major: nil, minor: nil, patch: nil, branch: nil, hash: nil, 
        year: nil, month: nil, day: nil
  end

  def run(argv) do
    check_env()
    Mix.Shell.IO.info [:green, "Rustler environment verified"]
  end

  def check_env() do
    nif_version = :erlang.system_info(:nif_version)
    unless to_string(nif_version) in @supported_nif_versions do
      Mix.raise "NIF version #{nif_version} is not supported by Rustler"
    end

    rustc_exec = locate_executable!("rustc")
    cargo_exec = locate_executable!("cargo")

    rustc_version = parse_rustc_version(cmd_expect!(rustc_exec, ["--version"]))
    check_rust_version!(rustc_version)

    case :os.type do
      {:unix, _} -> nil
      _ -> Mix.Shell.IO.info [:yellow, "Rustler is only currently properly tested with Linux. Proceed with caution."]
    end
  end

  defp cmd_expect!(prog, args) do
    case System.cmd(prog, args) do
      {out, 0} -> out
      {out, code} ->
        Mix.Shell.IO.error "#{prog} output: \n#{out}"
        Mix.raise "#{prog} failed with exit code: #{code}"
    end
  end

  defp locate_executable!(name) do
    case System.find_executable(name) do
      nil -> Mix.raise "Could not find '#{name}'."
      path -> path
    end
  end

  # TODO: Validate version properly
  def check_rust_version!(%RustCVersion{branch: "nightly"}), do: :ok
  def check_rust_version!(%RustCVersion{branch: branch}) do
    Mix.raise "Rustler needs the rust nightly branch, you have #{branch} installed."
  end

  @rustc_version_regex ~r/^rustc (\d+).(\d+).(\d+)-(\S+) \((\w+) (\d+)-(\d+)-(\d+)\)/
  defp parse_rustc_version(version_string) do
    match = Regex.run(@rustc_version_regex, version_string)
    [_, major, minor, patch, branch, hash, year, month, day] = match
    %RustCVersion{
      major: parse_int!(major), minor: parse_int!(minor), patch: parse_int!(patch),
      branch: branch, hash: hash,
      year: parse_int!(year), month: parse_int!(month), day: parse_int!(day)
    }
  end
  defp parse_int!(str) do
    {int, ""} = Integer.parse(str)
    int
  end

end
