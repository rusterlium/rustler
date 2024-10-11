defmodule PanicTest do
  use ExUnit.Case

  test "panics in NIFs are caught" do
    assert_raise ErlangError, &RustlerTest.panic_in_nif/0
    assert_raise ErlangError, &RustlerTest.panic_in_encode/0

    assert_raise ErlangError, fn ->
      RustlerTest.panic_in_decode(:anything)
    end
  end
end
