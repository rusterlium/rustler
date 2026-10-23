defmodule Rustler.ExDoc.Signature do
  @moduledoc false

  @spec item(map(), String.t(), map()) :: String.t()
  def item(%{"inner" => %{"variant" => variant}}, name, index) do
    fields =
      case variant["kind"] do
        "plain" ->
          ""

        %{"tuple" => ids} ->
          "(" <> Enum.map_join(ids, ", ", &field_type(&1, index)) <> ")"

        %{"struct" => %{"fields" => ids}} ->
          fields =
            Enum.map_join(ids, ", ", fn id ->
              field = Map.fetch!(index, to_string(id))
              field["name"] <> ": " <> field_type(id, index)
            end)

          " { " <> fields <> " }"
      end

    discriminant =
      if variant["discriminant"], do: " = " <> variant["discriminant"]["expr"], else: ""

    name <> fields <> discriminant
  end

  def item(item, name, _index), do: item(item, name)

  defp field_type(nil, _index), do: "_"
  defp field_type(id, index), do: type(Map.fetch!(index, to_string(id))["inner"]["struct_field"])

  @spec item(map(), String.t()) :: String.t()
  def item(%{"inner" => %{"function" => inner}}, name) do
    sig = inner["sig"]
    header = inner["header"]

    modifiers =
      for {key, value} <- [
            {"is_const", "const "},
            {"is_async", "async "},
            {"is_unsafe", "unsafe "}
          ],
          header[key],
          do: value

    inputs = Enum.map(sig["inputs"], &input/1)
    inputs = if sig["is_c_variadic"], do: inputs ++ ["..."], else: inputs
    output = if sig["output"], do: " -> " <> type(sig["output"]), else: ""

    Enum.join(modifiers) <>
      abi(Map.get(header, "abi", "Rust")) <>
      "fn " <>
      name <>
      generics(inner) <>
      "(" <> Enum.join(inputs, ", ") <> ")" <> output <> where_clause(inner)
  end

  def item(%{"inner" => %{"struct_field" => field}}, name), do: "#{name}: #{type(field)}"

  def item(%{"inner" => %{"type_alias" => inner}}, name),
    do: "type #{name}#{generics(inner)} = #{type(inner["type"])}"

  def item(%{"inner" => %{"assoc_type" => inner}}, name) do
    default = if inner["type"], do: " = " <> type(inner["type"]), else: ""
    "type #{name}#{generics(inner)}#{default}"
  end

  def item(%{"inner" => %{"constant" => inner}}, name),
    do: "const #{name}: #{type(inner["type"])} = #{inner["const"]["expr"]}"

  def item(%{"inner" => %{"assoc_const" => inner}}, name),
    do: "const #{name}: #{type(inner["type"])}"

  def item(%{"inner" => %{"static" => inner}}, name),
    do: "static #{if inner["is_mutable"], do: "mut ", else: ""}#{name}: #{type(inner["type"])}"

  def item(%{"inner" => %{"macro" => _}}, name), do: "macro_rules! #{name}"

  def item(%{"inner" => %{"proc_macro" => %{"kind" => "derive"}}}, name),
    do: "#[derive(#{name})]"

  def item(%{"inner" => %{"proc_macro" => %{"kind" => "attr"}}}, name), do: "#[#{name}]"
  def item(%{"inner" => %{"proc_macro" => %{"kind" => "bang"}}}, name), do: "#{name}!"
  def item(%{"inner" => %{"variant" => _}}, name), do: name

  def item(%{"inner" => inner}, name) do
    [{kind, data}] = Map.to_list(inner)
    kind <> " " <> name <> if(is_map(data), do: generics(data), else: "")
  end

  defp input(["self", %{"generic" => "Self"}]), do: "self"

  defp input(["self", %{"borrowed_ref" => %{"type" => %{"generic" => "Self"}}} = ref]),
    do: String.replace_suffix(type(ref), "Self", "self")

  defp input([name, t]), do: "#{name}: #{type(t)}"

  defp abi("Rust"), do: ""
  defp abi(abi) when is_binary(abi), do: "extern #{inspect(abi)} "

  defp abi(abi) when is_map(abi) do
    [{name, _unwind}] = Map.to_list(abi)
    "extern #{inspect(name)} "
  end

  defp generics(inner) do
    params = get_in(inner, ["generics", "params"]) || []
    params = Enum.reject(params, &get_in(&1, ["kind", "type", "is_synthetic"]))
    if params == [], do: "", else: "<" <> Enum.map_join(params, ", ", &param/1) <> ">"
  end

  defp param(%{"name" => name, "kind" => %{"type" => data}}) do
    default = if data["default"], do: " = " <> type(data["default"]), else: ""
    name <> bound_suffix(data["bounds"] || []) <> default
  end

  defp param(%{"name" => name, "kind" => %{"lifetime" => data}}),
    do: name <> bound_suffix(Enum.map(data["outlives"], &%{"outlives" => &1}))

  defp param(%{"name" => name, "kind" => %{"const" => data}}) do
    default = if data["default"], do: " = " <> data["default"], else: ""
    "const " <> name <> ": " <> type(data["type"]) <> default
  end

  defp bound_suffix([]), do: ""
  defp bound_suffix(values), do: ": " <> bounds(values)

  defp where_clause(inner) do
    predicates = get_in(inner, ["generics", "where_predicates"]) || []
    if predicates == [], do: "", else: " where " <> Enum.map_join(predicates, ", ", &predicate/1)
  end

  defp predicate(%{"bound_predicate" => predicate}),
    do: type(predicate["type"]) <> bound_suffix(predicate["bounds"])

  defp predicate(%{"lifetime_predicate" => predicate}),
    do:
      predicate["lifetime"] <> bound_suffix(Enum.map(predicate["outlives"], &%{"outlives" => &1}))

  defp predicate(%{"eq_predicate" => %{"lhs" => lhs, "rhs" => %{"type" => rhs}}}),
    do: type(lhs) <> " = " <> type(rhs)

  defp predicate(%{"eq_predicate" => %{"lhs" => lhs, "rhs" => %{"constant" => rhs}}}),
    do: type(lhs) <> " = " <> rhs["expr"]

  @spec type(map() | String.t()) :: String.t()
  def type(%{"primitive" => name}), do: name
  def type(%{"generic" => name}), do: name
  def type(%{"resolved_path" => path}), do: path["path"] <> args(path["args"])

  def type(%{"borrowed_ref" => ref}) do
    lifetime = if ref["lifetime"], do: ref["lifetime"] <> " ", else: ""
    "&" <> lifetime <> if(ref["is_mutable"], do: "mut ", else: "") <> type(ref["type"])
  end

  def type(%{"raw_pointer" => pointer}),
    do: "*" <> if(pointer["is_mutable"], do: "mut ", else: "const ") <> type(pointer["type"])

  def type(%{"tuple" => []}), do: "()"
  def type(%{"tuple" => [item]}), do: "(" <> type(item) <> ",)"
  def type(%{"tuple" => items}), do: "(" <> Enum.map_join(items, ", ", &type/1) <> ")"
  def type(%{"slice" => item}), do: "[" <> type(item) <> "]"
  def type(%{"array" => array}), do: "[#{type(array["type"])}; #{array["len"]}]"
  def type("infer"), do: "_"
  def type(%{"infer" => _}), do: "_"

  def type(%{"qualified_path" => path}) do
    trait = if path["trait"], do: " as " <> type(%{"resolved_path" => path["trait"]}), else: ""
    "<#{type(path["self_type"])}#{trait}>::#{path["name"]}#{args(path["args"])}"
  end

  def type(%{"function_pointer" => pointer}) do
    sig = pointer["sig"]
    output = if sig["output"], do: " -> " <> type(sig["output"]), else: ""
    "fn(" <> Enum.map_join(sig["inputs"], ", ", fn [_, t] -> type(t) end) <> ")" <> output
  end

  def type(%{"impl_trait" => bounds}), do: "impl " <> bounds(bounds)

  def type(%{"dyn_trait" => trait}) do
    traits = Enum.map_join(trait["traits"], " + ", &type(%{"resolved_path" => &1["trait"]}))
    "dyn " <> traits <> if(trait["lifetime"], do: " + " <> trait["lifetime"], else: "")
  end

  def type(%{"pat" => pat}), do: type(pat["type"]) <> " is " <> pat["__pat_unstable_do_not_use"]

  defp bounds(bounds) do
    Enum.map_join(bounds, " + ", fn
      %{"trait_bound" => bound} ->
        modifier =
          case bound["modifier"] do
            "maybe" -> "?"
            "maybe_const" -> "~const "
            _ -> ""
          end

        modifier <> type(%{"resolved_path" => bound["trait"]})

      %{"outlives" => lifetime} ->
        lifetime

      %{"use" => params} ->
        "use<" <> Enum.join(params, ", ") <> ">"
    end)
  end

  defp args(nil), do: ""

  defp args(%{"angle_bracketed" => inner}) do
    values =
      Enum.map(inner["args"], fn
        %{"type" => t} -> type(t)
        %{"lifetime" => lifetime} -> lifetime
        %{"const" => const} -> const["expr"]
        "infer" -> "_"
      end) ++ Enum.map(inner["constraints"], &constraint/1)

    if values == [], do: "", else: "<" <> Enum.join(values, ", ") <> ">"
  end

  defp args(%{"parenthesized" => inner}) do
    output = if inner["output"], do: " -> " <> type(inner["output"]), else: ""
    "(" <> Enum.map_join(inner["inputs"], ", ", &type/1) <> ")" <> output
  end

  defp constraint(%{"name" => name, "binding" => %{"equality" => %{"type" => t}}}),
    do: name <> " = " <> type(t)

  defp constraint(%{"name" => name, "binding" => %{"equality" => %{"constant" => c}}}),
    do: name <> " = " <> c["expr"]

  defp constraint(%{"name" => name, "binding" => %{"constraint" => bounds}}),
    do: name <> ": " <> bounds(bounds)
end
