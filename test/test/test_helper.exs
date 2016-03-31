defmodule NifNotLoadedError do
  defexception message: "nif not loaded"
end

defmodule TestLoadNif do

  defmacro __using__(opts) do
    module_name = opts[:module]
    {mod_name, _} = Code.eval_quoted(module_name)

    native_name = opts[:native]
    functions = opts[:functions]

    defmodule mod_name do

      @on_load :_load_nif

      quote do
        def _load_nif do
          :ok = :erlang.load_nif("./target/debug/lib#{unquote(native_name)}", [])
        end
      end
      |> Code.eval_quoted([], __ENV__)

      for {name, arity} <- functions do
        args = Enum.map(1..arity, &(Macro.var(String.to_atom("_#{&1}"), nil)))
        quote do
          def unquote(name)(unquote_splicing(args)), do: throw(NifNotLoadedError)
        end
      end
      |> Code.eval_quoted([], __ENV__)

    end

    #quote do
    #  ExUnit.Callbacks.setup_all do
    #    TestLoadNif.setup(unquote(module_name), unquote(native_name))
    #    on_exit fn -> TestLoadNif.exit(unquote(module_name)) end
    #    :ok
    #  end
    #end
    nil
  end

end
