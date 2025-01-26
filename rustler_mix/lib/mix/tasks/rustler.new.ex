defmodule Mix.Tasks.Rustler.New do
  use Mix.Task
  import Mix.Generator

  @shortdoc "Creates a new Rustler project."
  @moduledoc """
  Generates boilerplate for a new Rustler project.

  Usage:

  ```
  mix rustler.new [--module <Module>] [--name <Name>] [--otp-app <OTP App>]
  ```
  """

  @basic [
    {:eex, "basic/README.md", "README.md"},
    {:eex, "basic/Cargo.toml.eex", "Cargo.toml"},
    {:eex, "basic/src/lib.rs", "src/lib.rs"}
  ]

  @root [
    {:eex, "root/Cargo.toml.eex", "Cargo.toml"}
  ]

  @fallback_version "0.36.1"

  root = Path.join(:code.priv_dir(:rustler), "templates/")

  for {format, source, _} <- @basic ++ @root do
    if format != :keep do
      @external_resource Path.join(root, source)
      defp render(unquote(source)), do: unquote(File.read!(Path.join(root, source)))
    end
  end

  @switches [module: :string, name: :string, otp_app: :string]

  def run(argv) do
    {opts, _argv, _} = OptionParser.parse(argv, switches: @switches)

    module =
      case opts[:module] do
        nil ->
          prompt(
            "This is the name of the Elixir module the NIF module will be registered to.\n" <>
              "Module name"
          )

        module ->
          module
      end

    module_as_atom = parse_module_name!(module)

    name =
      case opts[:name] do
        nil ->
          prompt_default(
            "This is the name used for the generated Rust crate. The default is most likely fine.\n" <>
              "Library name",
            format_module_name_as_name(module)
          )

        name ->
          name
      end

    otp_app =
      case opts[:otp_app] do
        nil -> Mix.Project.config() |> Keyword.get(:app)
        otp_app -> otp_app
      end

    rustler_version = rustler_version()

    path = Path.join([File.cwd!(), "native", name])
    new(otp_app, path, module_as_atom, name, rustler_version, opts)

    if Path.join(File.cwd!(), "Cargo.toml") |> File.exists?() do
      Mix.shell().info([
        :green,
        "Workspace Cargo.toml already exists, please add ",
        :bright,
        path |> Path.relative_to_cwd(),
        :reset,
        :green,
        " to the ",
        :bright,
        "\"members\"",
        :reset,
        :green,
        " list"
      ])
    else
      copy_from(File.cwd!(), [library_name: name], @root)

      gitignore = Path.join(File.cwd!(), ".gitignore")

      if gitignore |> File.exists?() do
        Mix.shell().info([:green, "Updating .gitignore file"])
        File.write(gitignore, "\n# Rust binary artifacts\n/target/\n", [:append])
      else
        create_file(gitignore, "/target/\n")
      end
    end

    Mix.shell().info([
      :green,
      "\nReady to go! See #{path |> Path.relative_to_cwd()}/README.md for further instructions."
    ])
  end

  defp new(otp_app, path, module, name, rustler_version, _opts) do
    binding = [
      otp_app: otp_app,
      # Elixir syntax for the README
      module: module |> Macro.to_string(),
      # Erlang syntax for the init! invocation
      native_module: module |> Atom.to_string(),
      library_name: name,
      rustler_version: rustler_version
    ]

    copy_from(path, binding, @basic)
  end

  defp parse_module_name!(name) do
    case Code.string_to_quoted(name) do
      {:ok, atom} when is_atom(atom) ->
        atom

      {:ok, {:__aliases__, _, parts}} ->
        Module.concat(parts)

      _ ->
        Mix.raise(
          "Module name must be a valid Elixir alias (for example: Foo.Bar, or :foo_bar), got: #{inspect(name)}"
        )
    end
  end

  defp format_module_name_as_name(module_name) do
    if module_name |> String.starts_with?(":") do
      # Skip first
      module_name |> String.downcase() |> String.slice(1..-1//1)
    else
      module_name |> String.downcase() |> String.replace(".", "_")
    end
  end

  defp copy_from(target_dir, binding, mapping) when is_list(mapping) do
    for {format, source, target_path} <- mapping do
      target = Path.join(target_dir, target_path)

      case format do
        :keep ->
          File.mkdir_p!(target)

        :text ->
          create_file(target, render(source))

        :eex ->
          contents = EEx.eval_string(render(source), binding, file: source)
          create_file(target, contents)
      end
    end
  end

  defp prompt_default(message, default) do
    response = prompt([message, :white, " (", default, ")"])

    case response do
      "" -> default
      _ -> response
    end
  end

  defp prompt(message) do
    Mix.shell().print_app()
    resp = IO.gets(IO.ANSI.format([message, :white, " > "]))
    ?\n = :binary.last(resp)
    :binary.part(resp, {0, byte_size(resp) - 1})
  end

  @doc false
  defp rustler_version do
    versions =
      case Mix.Utils.read_path("https://crates.io/api/v1/crates/rustler",
             timeout: 10_000,
             unsafe_uri: true
           ) do
        {:ok, body} ->
          get_versions(body)

        err ->
          raise err
      end

    try do
      result =
        versions
        |> Enum.map(&Version.parse!/1)
        |> Enum.filter(&(&1.pre == []))
        |> Enum.max(Version)
        |> Version.to_string()

      Mix.shell().info("Fetched latest rustler crate version: #{result}")
      result
    rescue
      ex ->
        Mix.shell().error(
          "Failed to fetch rustler crate versions, using hardcoded fallback: #{@fallback_version}\nError: #{ex |> Kernel.inspect()}"
        )

        @fallback_version
    end
  end

  defp get_versions(data) do
    cond do
      # Erlang 27
      Code.ensure_loaded?(:json) and Kernel.function_exported?(:json, :decode, 1) ->
        data |> :json.decode() |> versions_from_parsed_json()

      Code.ensure_loaded?(Jason) ->
        data |> Jason.decode!() |> versions_from_parsed_json()

      true ->
        # Nasty hack: Instead of parsing the JSON, we use a regex, abusing the
        # compact nature of the returned data
        Regex.scan(~r/"num":"([^"]+)"/, data) |> Enum.map(fn [_, res] -> res end)
    end
  end

  defp versions_from_parsed_json(parsed) do
    parsed
    |> Map.fetch!("versions")
    |> Enum.filter(fn version -> not version["yanked"] end)
    |> Enum.map(fn version -> version["num"] end)
  end
end
