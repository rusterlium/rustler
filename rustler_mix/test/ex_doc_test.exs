defmodule Rustler.ExDocTest do
  use ExUnit.Case, async: true

  alias Rustler.ExDoc.{Retriever, Rust, Signature}

  @moduletag :tmp_dir

  setup_all do
    Application.ensure_all_started(:ex_doc)
    Application.ensure_all_started(:makeup_rust)
    :ok
  end

  setup %{tmp_dir: tmp} do
    function =
      item(3, "make", "function", %{
        "sig" => %{
          "inputs" => [["value", %{"primitive" => "u8"}]],
          "output" => %{"resolved_path" => %{"path" => "Thing", "args" => nil}},
          "is_c_variadic" => false
        },
        "generics" => %{"params" => []},
        "header" => %{}
      })

    items = [
      item(0, "sample", "module", %{"items" => [1, 2, 6, 8, 11]}),
      item(1, "private", "module", %{"items" => [3, 4, 12]}, "crate"),
      item(2, nil, "use", %{"id" => 4, "name" => "Renamed", "is_glob" => false}),
      function,
      item(4, "Thing", "struct", %{
        "kind" => %{"plain" => %{"fields" => [5, 7]}},
        "impls" => [9]
      }),
      item(5, "value", "struct_field", %{"primitive" => "u8"}),
      item(6, nil, "use", %{"id" => 1, "name" => "private", "is_glob" => true}),
      item(7, "secret", "struct_field", %{"primitive" => "u8"}, "crate"),
      item(8, nil, "use", %{"id" => 0, "name" => "cycle", "is_glob" => false}),
      item(9, nil, "impl", %{
        "items" => [10],
        "trait" => nil,
        "is_synthetic" => false,
        "blanket_impl" => nil
      }),
      %{function | "id" => 10, "name" => "new"},
      item(11, "Hidden", "struct", %{"kind" => "unit", "impls" => []}, "crate"),
      item(12, nil, "use", %{"id" => 1, "name" => "private", "is_glob" => true})
    ]

    data = %{
      "format_version" => 60,
      "root" => 0,
      "index" => Map.new(items, &{to_string(&1["id"]), &1}),
      "paths" => %{
        "0" => %{"path" => ["sample"], "kind" => "module"},
        "4" => %{"path" => ["sample", "Thing"], "kind" => "struct"}
      },
      "external_crates" => %{}
    }

    data =
      put_in(data, ["index", "0", "docs"], """
      # Sample

      See [`Thing`] and [make].
      Also see [the type](struct.Thing.html), [its method](struct.Thing.html#method.new),
      and [the module](index.html).

      ## Examples

      ```rust
      let value = 42;
      ```
      """)

    data = put_in(data, ["index", "0", "links"], %{"`Thing`" => 4, "make" => 3})
    path = Path.join(tmp, "sample.json")
    File.write!(path, JSON.encode!(data))

    config =
      ExDoc.Config.build(source_url_pattern: "https://example.test/%{path}#L%{line}")

    %{nodes: Retriever.docs_from_json(path, config), config: config, path: path}
  end

  test "builds nodes from public paths, aliases and cyclic glob exports", %{nodes: nodes} do
    assert Enum.map(nodes, & &1.id) == ["rust-sample", "rust-sample.Thing"]
    refute Enum.any?(nodes, &String.contains?(&1.id, "private"))
    root = Enum.find(nodes, &(&1.id == "rust-sample"))
    [function] = docs(root)
    assert function.signature == "fn make(value: u8) -> Thing"
    assert function.id == "function.make/1"

    renamed = Enum.find(nodes, &(&1.id == "rust-sample.Thing"))
    assert renamed.title == "sample::Thing"
    assert ExDoc.DocAST.text(renamed.doc) =~ "struct Thing"
    assert Enum.map(docs(renamed), & &1.name) == [:value, :new]
    assert renamed.source_url =~ "/rustler/src/example.rs#L10"
    assert %ExDoc.ModuleNode{language: Rust, metadata: %{rust_kind: "struct"}} = renamed
  end

  test "prefers fewer path segments while preserving modules and distinct type aliases", %{
    path: path,
    config: config
  } do
    data = path |> File.read!() |> JSON.decode!()
    module = item(13, "m", "module", %{"items" => [4, 14]})

    alias_type =
      item(14, "Alias", "type_alias", %{
        "type" => %{"resolved_path" => %{"path" => "Thing", "id" => 4, "args" => nil}},
        "generics" => %{"params" => []}
      })

    module_alias = item(15, nil, "use", %{"id" => 13, "name" => "other", "is_glob" => false})
    data = put_in(data, ["index", "0", "inner", "module", "items"], [13, 15, 2])
    data = put_in(data, ["index", "2", "inner", "use", "name"], "VeryLongPublicName")

    data =
      update_in(data, ["index"], fn index ->
        Map.merge(index, Map.new([module, alias_type, module_alias], &{to_string(&1["id"]), &1}))
      end)

    File.write!(path, JSON.encode!(data))
    nodes = Retriever.docs_from_json(path, config)

    assert Enum.map(nodes, & &1.id) == [
             "rust-sample",
             "rust-sample.VeryLongPublicName",
             "rust-sample.m",
             "rust-sample.m.Alias",
             "rust-sample.other"
           ]

    root = Enum.find(nodes, &(&1.id == "rust-sample"))
    html = root.doc |> Rust.autolink_doc(%ExDoc.Autolink{}) |> ExDoc.DocAST.to_html()
    assert html =~ ~s(href="rust-sample.VeryLongPublicName.html")
    assert html =~ ~s(href="rust-sample.VeryLongPublicName.html#function.new/1")

    aliases =
      for {id, name} <- [{16, "Zebra"}, {17, "Other"}],
          do: item(id, nil, "use", %{"id" => 4, "name" => name, "is_glob" => false})

    data = update_in(data, ["index", "0", "inner", "module", "items"], &(&1 ++ [16, 17]))

    data =
      update_in(
        data,
        ["index"],
        &Map.merge(&1, Map.new(aliases, fn item -> {to_string(item["id"]), item} end))
      )

    File.write!(path, JSON.encode!(data))
    ids = Enum.map(Retriever.docs_from_json(path, config), & &1.id)
    assert "rust-sample.Other" in ids
    refute "rust-sample.Zebra" in ids
    refute "rust-sample.VeryLongPublicName" in ids
  end

  test "resolves Rustdoc links to generated pages and member anchors", %{nodes: nodes} do
    root = Enum.find(nodes, &(&1.id == "rust-sample"))
    html = root.doc |> Rust.autolink_doc(%ExDoc.Autolink{ext: ".html"}) |> ExDoc.DocAST.to_html()

    assert html =~ ~s(href="rust-sample.Thing.html")
    assert html =~ ~s(href="rust-sample.html#function.make/1")
    assert html =~ ~s(href="rust-sample.Thing.html#function.new/1")
    assert html =~ ~s(href="rust-sample.html")
    refute html =~ "struct.Thing.html"
    refute html =~ ~s(href="index.html")
    assert html =~ ~s(id="module-examples")

    epub = root.doc |> Rust.autolink_doc(%ExDoc.Autolink{ext: ".xhtml"}) |> ExDoc.DocAST.to_html()
    assert epub =~ ~s(href="rust-sample.Thing.xhtml")
    refute epub =~ "rustdoc:"
  end

  test "keeps distinct generic trait implementations and enum payloads", %{
    path: path,
    config: config
  } do
    data = path |> File.read!() |> JSON.decode!()

    impls =
      for {id, type} <- [{13, "u8"}, {14, "u16"}] do
        item(id, nil, "impl", %{
          "items" => if(id == 13, do: [id + 2, 20], else: [id + 2]),
          "for" => %{"resolved_path" => %{"path" => "Thing", "args" => nil}},
          "trait" => %{
            "id" => 100,
            "path" => "From",
            "args" => %{
              "angle_bracketed" => %{
                "args" => [%{"type" => %{"primitive" => type}}],
                "constraints" => []
              }
            }
          },
          "is_synthetic" => false,
          "blanket_impl" => nil
        })
      end

    methods =
      for id <- [15, 16], do: %{data["index"]["3"] | "id" => id, "visibility" => "default"}

    enum = item(17, "Choice", "enum", %{"variants" => [18], "impls" => []})
    variant = item(18, "Some", "variant", %{"kind" => %{"tuple" => [5]}, "discriminant" => nil})

    marker =
      item(19, nil, "impl", %{
        "items" => [],
        "for" => %{"resolved_path" => %{"path" => "Thing", "args" => nil}},
        "trait" => %{"id" => 101, "path" => "Copy", "args" => nil},
        "is_synthetic" => false,
        "blanket_impl" => nil
      })

    associated_type =
      item(
        20,
        "Output",
        "assoc_type",
        %{
          "type" => %{"primitive" => "u8"},
          "generics" => %{"params" => []}
        },
        "default"
      )

    data = put_in(data, ["index", "4", "inner", "struct", "impls"], [9, 13, 14, 19])
    data = update_in(data, ["index", "0", "inner", "module", "items"], &(&1 ++ [17]))
    data = put_in(data, ["paths", "100"], %{"path" => ["core", "convert", "From"]})

    data =
      update_in(data, ["index"], fn index ->
        Map.merge(
          index,
          Map.new(
            impls ++ methods ++ [enum, variant, marker, associated_type],
            &{to_string(&1["id"]), &1}
          )
        )
      end)

    data = put_in(data, ["index", "13", "docs"], "Converts a byte.")

    File.write!(path, JSON.encode!(data))
    nodes = Retriever.docs_from_json(path, config)
    renamed = Enum.find(nodes, &(&1.id == "rust-sample.Thing"))
    ids = Enum.map(docs(renamed), & &1.id)
    assert length(ids) == length(Enum.uniq(ids))
    assert "function.<Thing as core::convert::From<u8>>::make/1" in ids
    assert "function.<Thing as core::convert::From<u16>>::make/1" in ids

    assert Enum.map(renamed.docs_groups, & &1.title) == [
             "Fields",
             "Functions",
             "impl Copy for Thing",
             "impl From [u16] for Thing",
             "impl From [u8] for Thing"
           ]

    functions = Enum.find(renamed.docs_groups, &(&1.title == "Functions"))
    assert [%ExDoc.DocNode{name: :new}] = functions.docs
    byte_impl = Enum.find(renamed.docs_groups, &(&1.title == "impl From [u8] for Thing"))
    assert ExDoc.DocAST.text(byte_impl.doc) =~ "Converts a byte."
    assert ExDoc.DocAST.text(byte_impl.doc) =~ "impl From<u8> for Thing"

    assert Enum.map(byte_impl.docs, & &1.signature) == [
             "type Output = u8",
             "fn make(value: u8) -> Thing"
           ]

    assert Enum.all?(byte_impl.docs, &(&1.name in [:make, :Output]))
    assert Enum.find(renamed.docs_groups, &(&1.title == "impl Copy for Thing")).docs == []

    output = Path.join(Path.dirname(path), "impl-docs")

    formatter =
      ExDoc.Formatter.Config.build("Sample", "1.0.0", output: output, formatters: ["html"])

    ExDoc.Formatter.run(Rustler.ExDoc.HTML, formatter, nodes, [], [])
    html = File.read!(Path.join(output, "rust-sample.Thing.html"))
    assert html =~ ~s(id="impl-from-u8-for-thing")
    assert html =~ "impl From [u8] for Thing"
    assert html =~ "fn make(value: u8) -&gt; Thing"
    refute html =~ "fn &lt;Thing as core::convert::From"
    choice = Enum.find(nodes, &(&1.id == "rust-sample.Choice"))
    assert [%ExDoc.DocNode{signature: "Some(u8)"}] = docs(choice)
  end

  test "disambiguates sections for traits with the same short name", %{path: path, config: config} do
    data = path |> File.read!() |> JSON.decode!()

    impls =
      for id <- [13, 14] do
        item(id, nil, "impl", %{
          "items" => [],
          "for" => %{"resolved_path" => %{"path" => "Thing", "args" => nil}},
          "trait" => %{"id" => id + 100, "path" => "Error", "args" => nil},
          "is_synthetic" => false,
          "blanket_impl" => nil
        })
      end

    data = put_in(data, ["index", "4", "inner", "struct", "impls"], [9, 13, 14])

    data =
      update_in(
        data,
        ["index"],
        &Map.merge(&1, Map.new(impls, fn impl -> {to_string(impl["id"]), impl} end))
      )

    data = put_in(data, ["paths", "113"], %{"path" => ["serde", "de", "Error"]})
    data = put_in(data, ["paths", "114"], %{"path" => ["serde", "ser", "Error"]})
    File.write!(path, JSON.encode!(data))
    renamed = Enum.find(Retriever.docs_from_json(path, config), &(&1.id == "rust-sample.Thing"))

    assert Enum.map(renamed.docs_groups, & &1.title) == [
             "Fields",
             "Functions",
             "impl serde::de::Error for Thing",
             "impl serde::ser::Error for Thing"
           ]
  end

  test "preserves receiver types, generic bounds and where clauses", %{path: path} do
    function = path |> File.read!() |> JSON.decode!() |> get_in(["index", "3"])

    bound = %{
      "trait_bound" => %{
        "trait" => %{"path" => "Clone", "args" => nil},
        "modifier" => "none"
      }
    }

    function =
      put_in(function, ["inner", "function", "generics"], %{
        "params" => [
          %{"name" => "T", "kind" => %{"type" => %{"bounds" => [bound], "default" => nil}}}
        ],
        "where_predicates" => [
          %{"bound_predicate" => %{"type" => %{"generic" => "T"}, "bounds" => [bound]}}
        ]
      })

    function =
      put_in(function, ["inner", "function", "sig", "inputs"], [
        [
          "self",
          %{"borrowed_ref" => %{"type" => %{"generic" => "Self"}, "is_mutable" => true}}
        ],
        ["value", %{"generic" => "T"}]
      ])

    assert Signature.item(function, "make") ==
             "fn make<T: Clone>(&mut self, value: T) -> Thing where T: Clone"

    derive = item(100, "Thing", "proc_macro", %{"kind" => "derive"})
    assert Signature.item(derive, "Thing") == "#[derive(Thing)]"
  end

  test "renders real ExDoc HTML and indexes Rust items", %{nodes: nodes, tmp_dir: tmp} do
    output = Path.join(tmp, "docs")

    elixir = %ExDoc.ModuleNode{
      id: "Example",
      title: "Example",
      module: Example,
      type: :module,
      language: ExDoc.Language.Elixir,
      doc: [],
      moduledoc_file: "example.ex",
      moduledoc_line: 1,
      metadata: %{}
    }

    config =
      ExDoc.Formatter.Config.build("Sample", "1.0.0",
        output: output,
        formatters: ["html"]
      )

    assert ExDoc.Formatter.run(Rustler.ExDoc.HTML, config, [elixir | nodes], [], [])
           |> Path.expand() ==
             Path.join(output, "index.html")

    root = File.read!(Path.join(output, "rust-sample.html"))
    assert root =~ ~s(id="function.make/1")
    assert root =~ ~s(href="rust-sample.Thing.html")
    assert root =~ "makeup rust"
    refute root =~ "rustdoc:"
    assert File.read!(Path.join(output, "rust-sample.Thing.html")) =~ "fn new"
    refute Enum.any?(File.ls!(output), &String.contains?(&1, "::"))
    [_, href] = Regex.run(~r/href="([^"]*Thing\.html)"/, root)
    assert URI.parse(href).scheme == nil
    assert File.regular?(Path.join(output, URI.decode(href)))

    rust_reference = File.read!(Path.join(output, "rust-api-reference.html"))
    elixir_reference = File.read!(Path.join(output, "elixir-api-reference.html"))
    assert rust_reference =~ ~s(href="rust-sample.Thing.html")
    refute rust_reference =~ ~s(href="Example.html")
    assert elixir_reference =~ ~s(href="Example.html")
    refute elixir_reference =~ ~s(href="rust-sample.Thing.html")
    assert File.read!(Path.join(output, "api-reference.html")) =~ "elixir-api-reference.html"

    [sidebar] = Path.wildcard(Path.join(output, "dist/sidebar_items-*.js"))

    sidebar =
      sidebar |> File.read!() |> String.replace_prefix("sidebarNodes=", "") |> JSON.decode!()

    assert Enum.any?(sidebar["modules"], &(&1["id"] == "Example" and &1["group"] == "Elixir API"))

    assert Enum.any?(
             sidebar["modules"],
             &(&1["id"] == "rust-sample" and &1["group"] == "Rust API")
           )

    assert Enum.any?(sidebar["extras"], &(&1["id"] == "rust-api-reference"))
    assert Enum.any?(sidebar["extras"], &(&1["id"] == "elixir-api-reference"))

    [search] = Path.wildcard(Path.join(output, "dist/search_data-*.js"))
    search = search |> File.read!() |> String.replace_prefix("searchData=", "") |> JSON.decode!()
    assert Enum.any?(search["items"], &(&1["ref"] == "rust-sample.Thing.html"))
  end

  test "reports missing and malformed input instead of omitting Rust docs", %{
    tmp_dir: tmp,
    config: config
  } do
    assert_raise ArgumentError, ~r/mix docs/, fn ->
      Retriever.docs_from_json(Path.join(tmp, "missing.json"), config)
    end

    invalid = Path.join(tmp, "invalid.json")
    File.write!(invalid, "{")
    assert_raise JSON.DecodeError, fn -> Retriever.docs_from_json(invalid, config) end
  end

  defp docs(node), do: Enum.flat_map(node.docs_groups, & &1.docs)

  defp item(id, name, kind, inner, visibility \\ "public") do
    %{
      "id" => id,
      "crate_id" => 0,
      "name" => name,
      "visibility" => visibility,
      "inner" => %{kind => inner},
      "docs" => nil,
      "links" => %{},
      "deprecation" => nil,
      "span" => %{"filename" => "rustler/src/example.rs", "begin" => [10, 1]}
    }
  end
end

defmodule Rustler.ExDocGenerationTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Rustler.ExDoc.Retriever

  @moduletag :tmp_dir
  @root Path.expand("../..", __DIR__)

  setup %{tmp_dir: tmp} do
    path = System.fetch_env!("PATH")
    System.put_env("PATH", tmp)
    on_exit(fn -> System.put_env("PATH", path) end)
    %{cargo: Path.join(tmp, "cargo")}
  end

  test "generates JSON in the repository target directory with Rustdoc flags", %{cargo: cargo} do
    File.write!(cargo, """
    #!/bin/sh
    printf '%s\\n' "$@" > "$0.args"
    printf '%s\\n' "$PWD" > "$0.cwd"
    printf '%s\\n' "$RUSTC_BOOTSTRAP" "$RUSTDOCFLAGS" > "$0.env"
    printf 'Generating Rustdoc JSON\\n'
    """)

    File.chmod!(cargo, 0o755)

    assert capture_io(fn -> assert Retriever.generate_json!() == :ok end) =~
             "Generating Rustdoc JSON"

    assert File.read!(cargo <> ".args") ==
             "doc\n--no-deps\n--all-features\n--target-dir\n#{@root}/target\n"

    assert File.read!(cargo <> ".cwd") == @root <> "\n"
    assert File.read!(cargo <> ".env") == "1\n-Z unstable-options --output-format=json\n"
  end

  test "stops retrieval on Cargo failure rather than reading stale JSON", %{cargo: cargo} do
    File.write!(cargo, "#!/bin/sh\nprintf 'Cargo failed\\n' >&2\nexit 23\n")
    File.chmod!(cargo, 0o755)

    assert capture_io(fn ->
             assert_raise Mix.Error, ~r/cargo exit code 23/, fn ->
               Retriever.docs_from_dir([], ExDoc.Config.build([]))
             end
           end) =~ "Cargo failed"
  end
end
