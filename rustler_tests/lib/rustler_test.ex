defmodule NifNotLoadedError do
  defexception message: "nif not loaded"
end

defmodule RustlerTest.Helper do
  def nif_feature_from_running_version() do
    [major, minor | _] =
      :erlang.system_info(:nif_version)
      |> to_string
      |> String.split(".")

    "nif_version_#{major}_#{minor}"
  end
end

defmodule RustlerTest do
  use Rustler,
    otp_app: :rustler_test,
    crate: :rustler_test,
    features: [RustlerTest.Helper.nif_feature_from_running_version()]

  defp err, do: :erlang.nif_error(:nif_not_loaded)

  def add_u32(_, _), do: err()
  def add_i32(_, _), do: err()
  def echo_u8(_), do: err()
  def option_inc(_), do: err()
  def erlang_option_inc(_), do: err()
  def result_to_int(_), do: err()

  def sum_list(_), do: err()
  def make_list(), do: err()

  def term_debug(_), do: err()

  def term_debug_and_reparse(term) do
    with debug_str <- term_debug(term),
         debug_str <- :erlang.binary_to_list(debug_str) ++ '.',
         {:ok, tokens, _} <- :erl_scan.string(debug_str),
         {:ok, ast} <- :erl_parse.parse_exprs(tokens),
         {:value, res, _} <- :erl_eval.exprs(ast, %{}) do
      res
    end
  end

  def term_eq(_, _), do: err()
  def term_cmp(_, _), do: err()
  def term_internal_hash(_, _), do: err()
  def term_phash2_hash(_), do: err()

  def sum_map_values(_), do: err()
  def map_entries_sorted(_), do: err()
  def map_from_arrays(_keys, _values), do: err()
  def map_from_pairs(_pairs), do: err()
  def map_generic(_), do: err()

  def resource_make(), do: err()
  def resource_set_integer_field(_, _), do: err()
  def resource_get_integer_field(_), do: err()
  def resource_make_immutable(_), do: err()
  def resource_immutable_count(), do: err()
  def monitor_resource_make(), do: err()
  def monitor_resource_monitor(_, _), do: err()
  def monitor_resource_down_called(_), do: err()
  def monitor_resource_demonitor(_), do: err()

  def resource_make_with_binaries(), do: err()
  def resource_make_binaries(_), do: err()

  def make_shorter_subbinary(_), do: err()
  def parse_integer(_), do: err()
  def binary_new(), do: err()
  def owned_binary_new(), do: err()
  def new_binary_new(), do: err()
  def unowned_to_owned(_), do: err()
  def realloc_shrink(), do: err()
  def realloc_grow(), do: err()
  def encode_string(), do: err()
  def decode_iolist(_), do: err()

  def atom_to_string(_), do: err()
  def atom_equals_ok(_), do: err()
  def binary_to_atom(_), do: err()
  def binary_to_existing_atom(_), do: err()

  def threaded_fac(_), do: err()
  def threaded_sleep(_), do: err()

  def send_all(_, _), do: err()
  def whereis_pid(_), do: err()
  def sublists(_), do: err()

  def tuple_echo(_), do: err()
  def record_echo(_), do: err()
  def map_echo(_), do: err()
  def exception_echo(_), do: err()
  def struct_echo(_), do: err()
  def unit_enum_echo(_), do: err()
  def tagged_enum_1_echo(_), do: err()
  def tagged_enum_2_echo(_), do: err()
  def tagged_enum_3_echo(_), do: err()
  def tagged_enum_4_echo(_), do: err()
  def untagged_enum_echo(_), do: err()
  def untagged_enum_with_truthy(_), do: err()
  def untagged_enum_for_issue_370(_), do: err()
  def newtype_echo(_), do: err()
  def tuplestruct_echo(_), do: err()
  def newtype_record_echo(_), do: err()
  def tuplestruct_record_echo(_), do: err()
  def reserved_keywords_type_echo(_), do: err()

  def dirty_io(), do: err()
  def dirty_cpu(), do: err()

  def sum_range(_), do: err()

  def bad_arg_error(), do: err()
  def atom_str_error(), do: err()
  def raise_atom_error(), do: err()
  def raise_term_with_string_error(), do: err()
  def raise_term_with_atom_error(), do: err()
  def term_with_tuple_error(), do: err()

  def nif_attrs_can_rename(), do: err()

  def add_from_tuple(_tuple), do: err()
  def add_one_to_tuple(_tuple), do: err()
  def join_tuple_elements(_tuple), do: err()
  def maybe_add_one_to_tuple(_tuple), do: err()
  def add_i32_from_tuple(_tuple), do: err()
  def greeting_person_from_tuple(_tuple), do: err()
end
