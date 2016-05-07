# Rustler

[Documentation](http://rustler.rustbridge.io/) | [Getting Started](https://github.com/hansihe/Rustler/blob/master/README.md#getting-started) | [Example](https://github.com/hansihe/Rustler_Example)

[![Build Status](https://travis-ci.org/hansihe/Rustler.svg?branch=master)](https://travis-ci.org/hansihe/Rustler)

Rustler is a library for writing Erlang NIFs in safe Rust code. That means
there should be no ways to crash the BEAM (Erlang VM). The library provides
facilities for generating the boilerplate for interacting with the BEAM,
handles encoding and decoding of Erlang terms, and catches rust panics before
they unwind into C.

The library provides functionality for both Erlang and Elixir, however Elixir
is favored as of now.

#### Features:
* Safety - The code you write in a Rust NIF should never be able to crash the BEAM.
* Interop - Decoding and encoding rust values into Erlang terms is as easy as a function call.
* Type composition - Making a Rust struct encodable and decodable to Erlang or Elixir can be done with a single attribute.
* Resource objects - Enables you to safely pass a reference to a Rust struct into Erlang code. The struct will be automatically dropped when it's no longer referenced.

#### Getting started
The easiest way of getting started, is using the Mix project generator.

* Run `mix archive.install https://github.com/hansihe/rustler_archives/raw/master/rustler_installer.ez` to install the tool.
* Run `mix rustler.new <path>` and follow the instructions.

NOTE: At the moment you need Rust nightly-2016-05-07. With [multirust](https://github.com/brson/multirust), you need to run `multirust override nightly-2016-05-07` in the generated project directory for things to work as expected.

#### How it looks like
This is the code for a minimal NIF that adds two numbers and returns the result.
```rust
#![feature(plugin)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
use rustler::{ NifEnv, NifTerm, NifResult, NifEncoder };

rustler_export_nifs!(
    "Elixir.TestNifModule",
    [("add", 2, add)],
    None
);

fn add<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let num1: i64 = try!(args[0].decode());
    let num2: i64 = try!(args[1].decode());
    Ok((num1 + num2).encode(env))
}
```

#### License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

##### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.

