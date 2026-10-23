defmodule Rustler.ExDoc.Rust do
  @moduledoc false

  @spec page_id(String.t()) :: String.t()
  def page_id(path), do: "rust-" <> String.replace(path, "::", ".")

  @spec doc_path(map()) :: String.t()
  def doc_path(%{"path" => path, "kind" => kind}) do
    {parents, [name]} = Enum.split(path, -1)

    prefix =
      case kind do
        "function" -> "fn"
        "constant" -> "constant"
        "proc_derive" -> "derive"
        "proc_attribute" -> "attr"
        "extern_type" -> "foreigntype"
        other -> other
      end

    parts =
      if kind == "module", do: path ++ ["index.html"], else: parents ++ ["#{prefix}.#{name}.html"]

    Enum.join(parts, "/")
  end

  @spec autolink_doc(ExDoc.DocAST.t(), ExDoc.Autolink.t()) :: ExDoc.DocAST.t()
  def autolink_doc(doc, config) do
    ExDoc.DocAST.map_tags(doc, fn
      {:a, attrs, inner, meta} = node ->
        case attrs[:href] do
          "rustdoc:" <> target ->
            [page | fragment] = String.split(target, "#", parts: 2)
            href = URI.encode(page) <> config.ext
            href = if fragment == [], do: href, else: href <> "#" <> hd(fragment)
            {:a, Keyword.put(attrs, :href, href), inner, meta}

          _ ->
            node
        end

      node ->
        node
    end)
  end

  @spec highlight_info() :: map()
  def highlight_info, do: %{language_name: "rust", lexer: Makeup.Lexers.RustLexer, opts: []}

  @spec autolink_spec(term(), ExDoc.Autolink.t()) :: iodata()
  def autolink_spec(spec, _config), do: ExDoc.Utils.h(spec)

  @spec format_spec(term()) :: iodata()
  def format_spec(spec), do: spec

  @spec format_spec_attribute(ExDoc.DocNode.t()) :: String.t()
  def format_spec_attribute(_node), do: ""
end
