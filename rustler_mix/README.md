# Rustler

This is the Mix package for [rustler](https://github.com/rusterlium/rustler), a library to write Erlang NIFs in
safe Rust code. Here, we provide the basic functionality to use Rustler from Elixir:

- A task to generate a new crate to write NIFs (`mix help rustler.new`)

See below for information on how to install this, which options are exposed through the configuration, and how to
load a NIF.

## Installation

This package is available on [`hex.pm`](https://hex.pm/packages/rustler). To install it, add it to your dependencies:

```elixir
def deps do
  [{:rustler, "~> 0.21.0", runtime: false}]
end
```

Then,

1. Run `mix deps.get` to fetch the dependency.
1. Run `mix rustler.new` and follow the instructions to generate the boilerplate for your NIF.
1. Load the NIF in your program. [See below](#loading-the-nif).

## Crate configuration

The `rustler_crates` configuration is a keyword list mapping the crate name (an atom) to the NIF configuration (another keyword list).
The NIF configuration may contain the following entries:

- `path` - The path to the crate directory relative to the project root
  (default: `native/<crate-name>`)
- `cargo` (:system default) - The rust/cargo version to build the NIF with. May be one of the following:
  - `:system` - Use the version installed on the system.
  - `{:rustup, "rust-version"}` - Use `rustup` to compile the NIF with a specific version.
  - `{:bin, "path"}` - Use `path` as the cargo command. This is not portable, and you should not normally use this.
- `default_features` (true default) - Boolean indicating if you want the NIF built with or without default cargo features.
- `features` ([] default) - List of binaries indicating what cargo features you want enabled when building.
- `target` (nil default) - Specify which build target to compile for. See .
- `mode` - Indicates what cargo build flavor to compile with. The default
  depends on the `Mix.env()`. When `:prod` the crate will be compiled in
  `release` mode. Otherwise it will be compiled in `debug` mode.
  - `:release` - Optimized build, normally a LOT faster than debug.
  - `:debug` - Unoptimized debug build with debug assertions and more.

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
