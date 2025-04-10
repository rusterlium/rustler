# Rustler

[Documentation](https://docs.rs/rustler/latest/rustler) | [Getting Started](https://github.com/rusterlium/rustler/blob/master/README.md#getting-started) | [Example](https://github.com/rusterlium/NifIo)

[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/rusterlium/rustler/main.yml)](https://github.com/rusterlium/rustler/actions/workflows/main.yml)
[![Hex.pm package version](https://img.shields.io/hexpm/v/rustler.svg)](https://hex.pm/packages/rustler)
[![Crates.io package version](https://img.shields.io/crates/v/rustler.svg)](https://crates.io/crates/rustler)
[![Last Updated](https://img.shields.io/github/last-commit/rusterlium/rustler.svg)](https://github.com/rusterlium/rustler/commits/master)

Rustler is a library for writing Erlang NIFs in safe Rust code. That means
there should be no ways to crash the BEAM (Erlang VM). The library provides
facilities for generating the boilerplate for interacting with the BEAM,
handles encoding and decoding of Erlang terms, and catches rust panics before
they unwind into C.

The library provides functionality for both Erlang and Elixir, however Elixir
is favored as of now.

#### Features

**Safety**

The code you write in a Rust NIF should never be able to crash the BEAM.

**Interop**

Decoding and encoding rust values into Erlang terms is as easy as a function call.

**Type composition**

Making a Rust struct encodable and decodable to Erlang or Elixir can be done with a single attribute.

**Resource objects**

Enables you to safely pass a reference to a Rust struct into Erlang code. The struct will be automatically dropped when it's no longer referenced.

#### Getting started

The easiest way of getting started is the [rustler Elixir library](https://hex.pm/packages/rustler).

- Add the [rustler Elixir library](https://hex.pm/packages/rustler) as a
  dependency of your project.
- Run `mix rustler.new` to generate a new NIF in your project. Follow the
  instructions.
- If you are already using [`serde`](https://serde.rs) and/or have been using
  `serde_rustler` before, please enable the `serde` feature in your NIF crate's
  `Cargo.toml` on the `rustler` dependency.

#### What it looks like

This is the code for a minimal NIF that adds two numbers and returns the result.

```rust
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

rustler::init!("Elixir.Math");
```

#### Minimal Supported Rust Version (MSRV)

Rustler currently has a minimal supported Rust version (MSRV) of 1.70. This
is the configured version in `.clippy.toml`.

#### Supported OTP and Elixir Versions

Rustler aims to support the newest three major OTP versions as well as newest three minor Elixir versions.

#### Supported NIF version

The minimal supported NIF version for a library should be defined via Cargo
features. The default is currently `2.15` (Erlang/OTP 22). To use features from
NIF version `2.16` (Erlang/OTP 24) or `2.17` (Erlang/OTP 26), the respective
feature flag has to be enabled on the dependency:

```toml
[dependencies]
rustler = { version = "...", features = ["nif_version_2_16"] }
```

#### Community

You can find us in the `#rustler:matrix.org` channel on [Matrix](https://matrix.to/#/#rustler:matrix.org)
or in the `#rustler` channel in [the Elixir lang Slack](https://elixir-slackin.herokuapp.com/).

#### License

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

##### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
