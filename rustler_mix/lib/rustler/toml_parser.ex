defmodule Rustler.TomlParser do
  # (very) Incomplete parser for TOML (https://github.com/toml-lang/toml)
  # Should be made into a separate project in the future. For now it is
  # just used to extract version numbers from Cargo.toml files.

  def parse(text) do
    {:ok, tokens, _test_chars} = text |> to_charlist |> :toml_lexer.string()
    {:ok, parsed} = :toml_parser.parse(tokens)

    collect(parsed, [])
  end

  # Pass 1: Collect

  def collect([{:kv, _, _} | _] = items, acc) do
    {rest, res_items} = collect_keys(items, [])
    collect(rest, [{:table, [], res_items} | acc])
  end

  def collect([{:table, path} | items], acc) do
    {rest, res_items, path_proc} = collect_table(path, items)
    collect(rest, [{:table, path_proc, res_items} | acc])
  end

  def collect([{:table_array, path} | items], acc) do
    {rest, res_items, path_proc} = collect_table(path, items)
    collect(rest, [{:table_array, path_proc, res_items} | acc])
  end

  def collect([], acc) do
    Enum.reverse(acc)
  end

  def collect_table(path, items) do
    {rest, res_items} = collect_keys(items, [])
    path_proc = Enum.map(path, &proc_key(&1))
    {rest, res_items, path_proc}
  end

  def collect_keys([{:kv, key, val} | rest], acc) do
    collect_keys(rest, [{proc_key(key), proc_val(val)} | acc])
  end

  def collect_keys(rest, acc) do
    {rest, acc}
  end

  def proc_key({:bare, name}) do
    name
  end

  def proc_key({:basic, name}) do
    name
  end

  def proc_key({:literal, name}) do
    name
  end

  def proc_val({:val_basic, raw_str}) do
    # TODO: Escapes
    :binary.part(raw_str, 1, byte_size(raw_str) - 2)
  end

  def proc_val({:val_integer, raw_num}) do
    {num, ""} = Integer.parse(raw_num)
    num
  end

  def proc_val({:val_boolean, bool}) do
    bool
  end

  def proc_val({:array, arr_ast}) do
    # TODO: Validate types
    Enum.map(arr_ast, fn
      item -> proc_val(item)
    end)
  end

  def proc_val(any) do
    any
  end

  # Pass 2

  def get_table_vals(data, path) do
    Enum.find_value(data, fn
      {:table, ^path, values} -> values
      {:table_array, ^path, values} -> values
      _ -> false
    end)
  end

  def get_keys_key(nil, _), do: nil

  def get_keys_key(vals, key) do
    case List.keyfind(vals, key, 0) do
      {_key, val} -> val
      _ -> nil
    end
  end

  def get_table_val(data, path, key) do
    vals = get_table_vals(data, path)
    get_keys_key(vals, key)
  end
end
