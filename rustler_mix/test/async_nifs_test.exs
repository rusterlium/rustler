defmodule Rustler.AsyncNifsTest do
  use ExUnit.Case, async: true

  test "async_nifs rejects malformed and duplicate entries" do
    for invalid <- [[echo: -1], [echo: 256], [echo: "one"], [echo: 1, echo: 1], :invalid] do
      assert_raise ArgumentError,
                   "async_nifs must be a keyword list of unique {name, arity} pairs",
                   fn -> compile_module(invalid, false) end
    end
  end

  test "async wrappers reject public NIF stubs rather than overwriting them" do
    assert_raise ArgumentError, "do not define a stub for async NIF echo/1", fn ->
      compile_module([echo: 1], true)
    end
  end

  defp compile_module(async_nifs, stub?) do
    module = Module.concat(__MODULE__, "Fixture#{System.unique_integer([:positive])}")

    stub =
      if stub? do
        quote do
          def echo(_), do: :stub
        end
      end

    Code.compile_quoted(
      quote do
        defmodule unquote(module) do
          use Rustler,
            otp_app: :rustler,
            skip_compilation?: true,
            async_nifs: unquote(Macro.escape(async_nifs))

          unquote(stub)
        end
      end
    )
  end
end
