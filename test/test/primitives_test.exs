defmodule PrimitivesTest do
  use ExUnit.Case, async: true
  use TestLoadNif, [
    module: PrimitivesTestNative, 
    native: "test_primitives", 
    functions: [
      add_u32: 2,
      add_i32: 2,
      tuple_add: 1,
    ],
  ]

  test "u8 decoding and encoding" do
    PrimitivesTestNative.test(1, 2)
  end
end
