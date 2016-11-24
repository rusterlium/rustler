defmodule NifNotLoadedError do
  defexception message: "nif not loaded"
end

defmodule RustlerTest do
  @on_load :load_nif

  def load_nif do
    require Rustler
    Rustler.load_nif(:rustler_test, "rustler_test")
  end

  defp err do
    throw NifNotLoadedError
  end

  def add_u32(_, _), do: err
  def add_i32(_, _), do: err
  def tuple_add(_), do: err
  def echo_u8(_), do: err

  def sum_list(_), do: err
  def make_list(), do: err

  def resource_make(), do: err
  def resource_set_integer_field(_, _), do: err
  def resource_get_integer_field(_), do: err

  def make_shorter_subbinary(_), do: err
end
