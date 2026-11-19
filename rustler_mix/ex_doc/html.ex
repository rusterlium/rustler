defmodule Rustler.ExDoc.HTML do
  @moduledoc false
  @behaviour ExDoc.Formatter

  alias ExDoc.{DocAST, ExtraNode}
  alias Rustler.ExDoc.Rust

  @impl true
  def autolink_options, do: ExDoc.Formatter.HTML.autolink_options()

  @impl true
  def run(config, nodes, extras) do
    {rust, elixir} = Enum.split_with(nodes, &(&1.language == Rust))

    references = [
      reference("Elixir API Reference", "elixir-api-reference", elixir),
      reference("Rust API Reference", "rust-api-reference", rust)
    ]

    nodes =
      Enum.map(nodes, fn node ->
        group = if node.language == Rust, do: "Rust API", else: "Elixir API"
        %{node | group: node.group || group}
      end)

    config = %{
      config
      | api_reference: false,
        main: if(config.main == "api-reference", do: "elixir-api-reference", else: config.main),
        redirects:
          config.redirects
          |> Map.new()
          |> Map.put_new("api-reference", "elixir-api-reference")
    }

    ExDoc.Formatter.HTML.run(config, nodes, references ++ extras)
  end

  defp reference(title, id, nodes) do
    {tasks, modules} = Enum.split_with(nodes, &(&1.type == :task))

    doc =
      for {heading, entries} <- [{"Modules", modules}, {"Mix Tasks", tasks}],
          entries != [],
          ast <- section(heading, entries),
          do: ast

    %ExtraNode{
      id: id,
      title: title,
      title_doc: title,
      group: "API References",
      type: :extra,
      doc: DocAST.add_ids_to_headers(doc, [:h2]),
      source_doc: "",
      search_data: [%{anchor: "", title: title, type: "extras", body: DocAST.text(doc, " ")}]
    }
  end

  defp section(heading, entries) do
    links =
      entries
      |> Enum.sort_by(& &1.title)
      |> Enum.map(fn node ->
        {:li, [], [{:a, [href: URI.encode(node.id) <> ".html"], [node.title], %{}}], %{}}
      end)

    [{:h2, [], [heading], %{}}, {:ul, [], links, %{}}]
  end
end
