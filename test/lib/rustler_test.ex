defmodule NifNotLoadedError do
  defexception message: "nif not loaded"
end

defmodule RustlerTest do
  use Rustler, otp_app: :rustler_test

  defp err do
    throw NifNotLoadedError
  end

  def add_u32(_, _), do: err()
  def add_i32(_, _), do: err()
  def tuple_add(_), do: err()
  def echo_u8(_), do: err()

  def sum_list(_), do: err()
  def make_list(), do: err()

  def sum_map_values(_), do: err()
  def map_entries_sorted(_), do: err()

  def resource_make(), do: err()
  def resource_set_integer_field(_, _), do: err()
  def resource_get_integer_field(_), do: err()
  def resource_make_immutable(_), do: err()
  def resource_immutable_count(), do: err()

  def make_shorter_subbinary(_), do: err()
  def parse_integer(_), do: err()

  def atom_to_string(_), do: err()
  def atom_equals_ok(_), do: err()

  def threaded_fac(_), do: err()
  def threaded_sleep(_), do: err()

  def send_all(_, _), do: err()
  def sublists(_), do: err()
end
