# Nif for <%= project_name %>

## To build the NIF module:

  * Make sure your projects `mix.exs` has the `:rustler` compiler listed in the `project` function: `compilers: [:rustler] ++ Mix.compilers`
  * Add your crate to the `rustler_crates` attribute in the `project function. [See here](https://hexdocs.pm/rustler/basics.html#crate-configuration).
  * Your NIF will now build along with your project.

## To load the NIF:

```elixir
defmodule <%= module %> do
    require Rustler

    @on_load :load_nif
    def load_nif do
        Rustler.load_nif("<%= library_name %>")
    end

    # When your NIF is loaded, it will override this function.
    def add(_a, _b), do: throw :nif_not_loaded
end
```

## Examples
[This](https://github.com/hansihe/NifIo) is a complete example of a NIF written in Rust.
