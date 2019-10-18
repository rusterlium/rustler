# Rustler

[Documentation](https://docs.rs/crate/rustler) | [Getting Started](https://github.com/rusterlium/rustler/blob/master/README.md#getting-started) | [Example](https://github.com/hansihe/NifIo)

![Build Status](https://github.com/rusterlium/rustler/workflows/CI/badge.svg?branch=master)

Rustler is a library for writing Erlang NIFs in safe Rust code. That means
there should be no ways to crash the BEAM (Erlang VM). The library provides
facilities for generating the boilerplate for interacting with the BEAM,
handles encoding and decoding of Erlang terms, and catches rust panics before
they unwind into C.

The library provides functionality for both Erlang and Elixir, however Elixir
is favored as of now.

#### Features:

- Safety - The code you write in a Rust NIF should never be able to crash the BEAM.
- Interop - Decoding and encoding rust values into Erlang terms is as easy as a function call.
- Type composition - Making a Rust struct encodable and decodable to Erlang or Elixir can be done with a single attribute.
- Resource objects - Enables you to safely pass a reference to a Rust struct into Erlang code. The struct will be automatically dropped when it's no longer referenced.

#### Getting started

The easiest way of getting started is the [rustler elixir library](https://hex.pm/packages/rustler).

- Add the [rustler elixir library](https://hex.pm/packages/rustler) as a dependency of your project.
- Run `mix rustler.new` to generate a new NIF in your project. Follow the instructions.
- If you're already using [`serde`](https://serde.rs), consider using [`serde_rustler`](https://github.com/sunny-g/serde_rustler/tree/master/serde_rustler) to easily encode and decode your data types into and from Elixir terms.

NOTE: If you have previously used Rustler, you need to run `mix archive.uninstall rustler_installer.ez` to remove it before generating the NIF.

#### What it looks like

This is the code for a minimal NIF that adds two numbers and returns the result.

```rust
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

rustler::init!("Elixir.Math", [add]);
```

#### Supported nif_version

Rustler uses `erlang:system_info(nif_version)` to detect the supported NIF version of the Erlang/OTP
system for which the NIF is to be compiled. It is possible to restrict the NIF version to an older
version if the NIF is to be compiled for an older version of Erlang. For example, if the target NIF
version should be `2.7` (Erlang/OTP 17.3), this can be defined using an environment variable:

```
RUSTLER_NIF_VERSION=2.7 mix compile
```

#### Community

You can find us in `#rustler` on [freenode](http://freenode.net/) or [the elixir-lang slack](https://elixir-slackin.herokuapp.com/).

#### License

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

##### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
