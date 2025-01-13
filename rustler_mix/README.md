# Rustler

[![Module Version](https://img.shields.io/hexpm/v/rustler.svg)](https://hex.pm/packages/rustler)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/rustler/)
[![Total Download](https://img.shields.io/hexpm/dt/rustler.svg)](https://hex.pm/packages/rustler)
[![License](https://img.shields.io/hexpm/l/rustler.svg)](https://github.com/rusterlium/rustler/blob/master/LICENSE)
[![Last Updated](https://img.shields.io/github/last-commit/rusterlium/rustler.svg)](https://github.com/rusterlium/rustler/commits/master)

The Mix package for [rustler](https://github.com/rusterlium/rustler), a library to write Erlang Native Implemented Functions (NIFs) in [Rust](https://www.rust-lang.org/) programming language.

## Installation

This package is available on [Hex.pm](https://hex.pm/packages/rustler). To install it, add `:rustler` to your dependencies:

```elixir
def deps do
  [
    {:rustler, "~> 0.36.0", runtime: false}
  ]
end
```

## Usage

1.  Fetch and compile all necessary dependencies:

    ```
    $ mix deps.get && mix deps.compile
    ```
2.  Check your installation by showing help from the installed Mix task:

    ```
    $ mix help rustler.new
    ```

3.  Generate the boilerplate for a new Rustler project. Follow the instructions
    to configure your project:

    ```
    $ mix rustler.new
    ```

4.  [Load the NIF in your program.](#loading-the-nif).

## Crate configuration

The Rust crate compilation can be controlled via Mix compile-time configuration in `config/config.exs`.
See [configuration options](https://hexdocs.pm/rustler/Rustler.html#module-configuration-options) for more details.


## Loading the NIF

Loading a Rustler NIF is done in almost the same way as normal NIFs.

The actual loading is done by calling `use Rustler, otp_app: :my_app` in the module you want to load the NIF in.
This sets up the `@on_load` module hook to load the NIF when the module is first
loaded.

```elixir
defmodule MyProject.MyModule do
  use Rustler,
    otp_app: :my_app,
    crate: :my_crate

  # When loading a NIF module, dummy clauses for all NIF function are required.
  # NIF dummies usually just error out when called when the NIF is not loaded, as that should never normally happen.
  def my_native_function(_arg1, _arg2), do: :erlang.nif_error(:nif_not_loaded)
end
```

Note that `:crate` is the name in the `[lib]` section of your `Cargo.toml`. The
`:crate` option is optional if your crate and `otp_app` use the same name.

See the `Rustler` module for more information.

## Copyright and License

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](../LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](../LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
