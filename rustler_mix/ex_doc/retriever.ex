defmodule Rustler.ExDoc.Retriever do
  @moduledoc false

  alias ExDoc.{DocAST, DocGroupNode, DocNode, ModuleNode}
  alias Rustler.ExDoc.{Rust, Signature}

  @json_dir Path.expand("../../target/doc", __DIR__)
  @pages ~w(module struct enum union trait type_alias primitive extern_type)
  @groups %{
    "function" => "Functions",
    "macro" => "Macros",
    "proc_macro" => "Macros",
    "constant" => "Constants",
    "static" => "Statics",
    "struct_field" => "Fields",
    "variant" => "Variants",
    "assoc_type" => "Associated Types",
    "assoc_const" => "Associated Constants"
  }

  @spec docs_from_dir([Path.t()], ExDoc.Config.t()) ::
          {[ModuleNode.t()], [ModuleNode.t()]}
  def docs_from_dir(dirs, config) do
    generate_json!()
    {modules, filtered} = ExDoc.Retriever.docs_from_dir(dirs, config)

    rust =
      Enum.flat_map(~w(rustler rustler_codegen), fn crate ->
        docs_from_json(Path.join(@json_dir, crate <> ".json"), config)
      end)

    {included, excluded} =
      Enum.split_with(rust, &config.filter_modules.(&1.module, &1.metadata))

    {modules ++ included, filtered ++ excluded}
  end

  @spec generate_json!() :: :ok
  def generate_json! do
    cargo =
      System.find_executable("cargo") || Mix.raise("Cargo is required to generate Rustdoc JSON")

    {_, status} =
      System.cmd(
        cargo,
        ["doc", "--no-deps", "--all-features", "--target-dir", Path.dirname(@json_dir)],
        cd: Path.expand("../..", __DIR__),
        env: [
          {"RUSTC_BOOTSTRAP", "1"},
          {"RUSTDOCFLAGS", "-Z unstable-options --output-format=json"}
        ],
        stderr_to_stdout: true,
        into: IO.stream()
      )

    if status != 0 do
      Mix.raise("Failed to generate Rustdoc JSON (cargo exit code #{status})")
    end

    :ok
  end

  @spec docs_from_json(Path.t(), ExDoc.Config.t()) :: [ModuleNode.t()]
  def docs_from_json(path, config) do
    unless File.exists?(path) do
      raise ArgumentError, "Rustdoc JSON not found: #{path}. Run `mix docs` to regenerate it."
    end

    data = path |> File.read!() |> JSON.decode!()
    %{"index" => index, "root" => root, "paths" => _paths, "format_version" => version} = data

    unless is_integer(version) do
      raise ArgumentError, "invalid Rustdoc JSON format_version in #{path}"
    end

    root = Map.fetch!(index, to_string(root))
    context = %{data: data, index: index, config: config, crate_id: root["crate_id"]}

    pages =
      root
      |> collect(root["name"], context, MapSet.new())
      |> Enum.sort_by(fn {_, path, _} ->
        {length(String.split(path, "::")), String.length(path), path}
      end)
      |> Enum.uniq_by(fn {item, path, _} ->
        if elem(kind(item), 0) == "module", do: {:module, path}, else: {:type, item["id"]}
      end)

    targets =
      Enum.reduce(pages, %{}, fn {item, page, entries}, acc ->
        page = Rust.page_id(page)
        acc = Map.put_new(acc, to_string(item["id"]), {page, nil})

        Enum.reduce(entries, acc, fn {entry, name}, acc ->
          Map.put_new(acc, to_string(entry["id"]), {page, anchor(entry, name)})
        end)
      end)

    context = Map.merge(context, %{targets: targets, routes: routes(pages, context)})

    pages
    |> Enum.uniq_by(fn {_, page, _} -> page end)
    |> Enum.map(&module_node(&1, context))
    |> Enum.sort_by(& &1.id)
  end

  defp collect(item, path, context, seen) do
    id = item["id"]

    if MapSet.member?(seen, id) do
      []
    else
      seen = MapSet.put(seen, id)
      {kind, _inner} = kind(item)

      cond do
        kind in @pages ->
          entries = entries(item, context)
          pages = [{item, path, entries}]

          if kind == "module" do
            pages ++
              (exports(item, context)
               |> Enum.filter(fn {child, _} -> elem(kind(child), 0) in @pages end)
               |> Enum.flat_map(fn {child, name} ->
                 collect(child, path <> "::" <> name, context, seen)
               end))
          else
            pages
          end

        true ->
          []
      end
    end
  end

  defp children(item, context) do
    {_, inner} = kind(item)
    lookup(Map.get(inner, "items", []), context)
  end

  defp exports(item, context, seen \\ MapSet.new()) do
    item
    |> children(context)
    |> Enum.filter(&public?/1)
    |> Enum.flat_map(&resolve_export(&1, context, seen))
  end

  defp resolve_export(%{"inner" => %{"use" => use}} = item, context, seen) do
    target = context.index[to_string(use["id"])]

    cond do
      is_nil(target) or MapSet.member?(seen, item["id"]) ->
        []

      use["is_glob"] ->
        exports(target, context, MapSet.put(seen, item["id"]))

      true ->
        resolve_export(target, context, MapSet.put(seen, item["id"]))
        |> Enum.map(fn {entry, _name} -> {entry, use["name"]} end)
    end
  end

  defp resolve_export(item, _context, _seen), do: [{item, item["name"]}]

  defp entries(item, context) do
    {kind, inner} = kind(item)

    direct =
      case kind do
        "module" -> []
        "trait" -> children(item, context)
        "enum" -> lookup(inner["variants"], context)
        "union" -> lookup(inner["fields"], context) |> Enum.filter(&public?/1)
        "struct" -> struct_fields(inner["kind"], context) |> Enum.filter(&public?/1)
        _ -> []
      end

    direct =
      if kind == "module",
        do: exports(item, context),
        else: Enum.map(direct, &{&1, &1["name"]})

    impls =
      for impl <- implementations(item, context),
          {"impl", body} = kind(impl),
          entry <- children(impl, context),
          public?(entry) or not is_nil(body["trait"]) do
        prefix = impl_prefix(body, context)
        entry = if body["trait"], do: Map.put(entry, :trait_impl, impl), else: entry
        {entry, prefix <> entry["name"]}
      end

    (direct ++ impls)
    |> Enum.filter(fn {entry, _} -> Map.has_key?(@groups, elem(kind(entry), 0)) end)
    |> Enum.uniq_by(fn {entry, name} -> {entry["id"], name} end)
  end

  defp impl_prefix(%{"trait" => nil}, _context), do: ""

  defp impl_prefix(%{"trait" => trait, "for" => type}, context) do
    trait = Signature.type(%{"resolved_path" => qualified_trait(trait, context)})
    "<#{Signature.type(type)} as #{trait}>::"
  end

  defp qualified_trait(trait, context) do
    path =
      case context.data["paths"][to_string(trait["id"])] do
        %{"path" => path} -> Enum.join(path, "::")
        nil -> trait["path"]
      end

    %{trait | "path" => path}
  end

  defp implementations(item, context) do
    {_, inner} = kind(item)

    for impl <- lookup(Map.get(inner, "impls", []), context),
        body = impl["inner"]["impl"],
        not body["is_synthetic"],
        is_nil(body["blanket_impl"]),
        do: impl
  end

  defp trait_groups(item, context) do
    impls = Enum.filter(implementations(item, context), & &1["inner"]["impl"]["trait"])
    titles = Enum.frequencies_by(impls, &ExDoc.Utils.text_to_id(impl_title(&1, context, false)))

    candidates =
      Enum.map(impls, fn impl ->
        short = impl_title(impl, context, false)

        title =
          if titles[ExDoc.Utils.text_to_id(short)] > 1,
            do: impl_title(impl, context, true),
            else: short

        {impl, title}
      end)

    ids = Enum.frequencies_by(candidates, fn {_, title} -> ExDoc.Utils.text_to_id(title) end)

    Map.new(candidates, fn {impl, title} ->
      title =
        if ids[ExDoc.Utils.text_to_id(title)] > 1,
          do: "#{title} (impl #{impl["id"]})",
          else: title

      {impl["id"], {title, impl}}
    end)
  end

  defp impl_title(impl, context, qualified?) do
    # ExDoc renders group titles as HTML, so use brackets for generic arguments.
    impl_declaration(impl, context, qualified?)
    |> String.replace("<", " [")
    |> String.replace(">", "]")
  end

  defp impl_declaration(impl, context, qualified?) do
    body = impl["inner"]["impl"]
    trait = if qualified?, do: qualified_trait(body["trait"], context), else: body["trait"]
    "impl #{Signature.type(%{"resolved_path" => trait})} for #{Signature.type(body["for"])}"
  end

  defp struct_fields(%{"plain" => %{"fields" => ids}}, context), do: lookup(ids, context)
  defp struct_fields(%{"tuple" => ids}, context), do: lookup(ids, context)
  defp struct_fields("unit", _context), do: []

  defp lookup(ids, context) do
    for id <- ids, not is_nil(id), do: Map.fetch!(context.index, to_string(id))
  end

  defp public?(item), do: item["visibility"] == "public"
  defp kind(item), do: item["inner"] |> Map.to_list() |> hd()

  defp module_node({item, page, entries}, context) do
    {kind, _} = kind(item)
    {file, line, url} = source(item, context.config)
    context = Map.put(context, :rustdoc_path, rustdoc_path(item["id"], context))
    trait_groups = trait_groups(item, context)
    context = Map.put(context, :trait_groups, trait_groups)

    docs =
      entries
      |> Enum.map(fn {entry, name} -> doc_node(entry, name, context) end)
      |> Enum.group_by(& &1.group)

    groups =
      docs
      |> Map.drop(Enum.map(trait_groups, fn {_, {title, _}} -> title end))
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(fn {title, nodes} ->
        %DocGroupNode{title: title, docs: Enum.sort_by(nodes, & &1.id)}
      end)

    groups =
      groups ++
        (trait_groups
         |> Map.values()
         |> Enum.sort_by(&elem(&1, 0))
         |> Enum.map(fn {title, impl} ->
           %DocGroupNode{
             title: title,
             doc:
               [
                 {:pre, [],
                  [{:code, [class: "rust"], [impl_declaration(impl, context, false)], %{}}], %{}}
               ] ++ doc(impl, context, ExDoc.Utils.text_to_id(title) <> "-"),
             docs: docs |> Map.get(title, []) |> Enum.sort_by(& &1.id)
           }
         end))

    declaration =
      if kind == "module" do
        []
      else
        name = page |> String.split("::") |> List.last()
        [{:pre, [], [{:code, [class: "rust"], [Signature.item(item, name)], %{}}], %{}}]
      end

    %ModuleNode{
      id: Rust.page_id(page),
      title: page,
      module: String.to_atom(page),
      type: :module,
      language: Rust,
      group: "Rust API",
      doc: declaration ++ doc(item, context, "module-"),
      source_doc: item["docs"],
      source_format: "text/markdown",
      moduledoc_file: file,
      moduledoc_line: line,
      source_path: file,
      source_url: url,
      deprecated: deprecated(item),
      docs_groups: groups,
      annotations: [kind],
      metadata: %{kind: :module, rust_kind: kind, deprecated: deprecated(item)}
    }
  end

  defp doc_node(item, name, context) do
    {kind, _} = kind(item)
    {file, line, url} = source(item, context.config)
    id = anchor(item, name)
    impl = item[:trait_impl]
    display_name = if impl, do: item["name"], else: name

    group =
      if impl,
        do: elem(Map.fetch!(context.trait_groups, impl["id"]), 0),
        else: Map.fetch!(@groups, kind)

    %DocNode{
      id: id,
      name: String.to_atom(display_name),
      arity: arity(item),
      type: if(kind in ~w(macro proc_macro), do: :macro, else: :function),
      signature: Signature.item(item, display_name, context.index),
      doc: doc(item, context, id <> "-"),
      source_doc: item["docs"],
      doc_file: file,
      doc_line: line,
      source_url: url,
      group: group,
      deprecated: deprecated(item),
      annotations: [kind]
    }
  end

  defp anchor(item, name), do: "#{elem(kind(item), 0)}.#{name}/#{arity(item)}"

  defp arity(%{"inner" => %{"function" => %{"sig" => sig}}}),
    do: length(sig["inputs"])

  defp arity(_), do: 0

  defp source(%{"span" => %{"filename" => file, "begin" => [line, _]}}, config) do
    file = file |> Path.expand(Path.expand("../..", __DIR__)) |> Path.relative_to_cwd()
    {file, line, config.source_url_pattern.(file, line)}
  end

  defp source(_, _), do: {"nofile", 1, nil}

  defp deprecated(%{"deprecation" => %{"note" => note}}), do: note || "Deprecated"
  defp deprecated(_), do: nil

  defp doc(%{"docs" => nil}, _context, _prefix), do: []

  defp doc(item, context, prefix) do
    definitions =
      Enum.flat_map(item["links"], fn {label, id} ->
        case link(id, context) do
          nil -> []
          url -> ["\n[#{label}]: #{url}"]
        end
      end)

    {file, line, _} = source(item, context.config)

    (item["docs"] <> "\n" <> Enum.join(definitions))
    |> DocAST.parse!("text/markdown",
      file: file,
      line: line,
      markdown_processor: context.config.markdown_processor
    )
    |> rewrite_relative_links(context)
    |> DocAST.add_ids_to_headers([:h2, :h3], prefix)
  end

  defp routes(pages, context) do
    Enum.reduce(pages, %{}, fn {item, page, entries}, routes ->
      case rustdoc_path(item["id"], context) do
        nil ->
          routes

        path ->
          page = Rust.page_id(page)
          routes = Map.put_new(routes, path, {page, nil})

          Enum.reduce(entries, routes, fn {entry, name}, routes ->
            target = {page, anchor(entry, name)}

            routes =
              case rustdoc_path(entry["id"], context) do
                nil -> routes
                entry_path -> Map.put_new(routes, entry_path, target)
              end

            fragments =
              case elem(kind(entry), 0) do
                "function" -> ["method", "tymethod"]
                "struct_field" -> ["structfield"]
                "variant" -> ["variant"]
                "assoc_type" -> ["associatedtype"]
                "assoc_const" -> ["associatedconstant"]
                _ -> []
              end

            Enum.reduce(fragments, routes, fn prefix, routes ->
              Map.put_new(routes, path <> "##{prefix}.#{entry["name"]}", target)
            end)
          end)
      end
    end)
  end

  defp rustdoc_path(id, context) do
    case context.data["paths"][to_string(id)] do
      nil -> nil
      summary -> Rust.doc_path(summary)
    end
  end

  defp rewrite_relative_links(ast, %{rustdoc_path: nil}), do: ast

  defp rewrite_relative_links(ast, context) do
    DocAST.map_tags(ast, fn
      {:a, attrs, inner, meta} = node ->
        uri = URI.parse(attrs[:href] || "")

        if is_nil(uri.scheme) and is_nil(uri.host) and uri.path not in [nil, ""] do
          uri = URI.merge("https://rustdoc.invalid/" <> context.rustdoc_path, uri)
          path = String.trim_leading(uri.path, "/")
          key = if uri.fragment, do: path <> "#" <> uri.fragment, else: path

          case context.routes[key] do
            nil -> node
            target -> {:a, Keyword.put(attrs, :href, target_link(target)), inner, meta}
          end
        else
          node
        end

      node ->
        node
    end)
  end

  defp link(id, context) do
    case context.targets[to_string(id)] do
      nil -> external_link(id, context)
      target -> target_link(target)
    end
  end

  defp target_link({page, nil}), do: "rustdoc:#{page}"
  defp target_link({page, anchor}), do: "rustdoc:#{page}##{URI.encode(anchor)}"

  defp external_link(id, context) do
    with %{"crate_id" => crate} = summary <-
           context.data["paths"][to_string(id)],
         true <- crate != context.crate_id,
         %{"html_root_url" => root} when is_binary(root) <-
           context.data["external_crates"][to_string(crate)] do
      String.trim_trailing(root, "/") <> "/" <> Rust.doc_path(summary)
    else
      _ -> nil
    end
  end
end
