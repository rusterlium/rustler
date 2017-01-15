# Rustler

[Documentation](http://rustler.rustbridge.io/) | [Getting Started](https://github.com/hansihe/Rustler/blob/master/README.md#getting-started) | [Example](https://github.com/hansihe/NifIo)

[![Build Status](https://travis-ci.org/hansihe/Rustler.svg?branch=master)](https://travis-ci.org/hansihe/Rustler)

Rustler is a library for writing Erlang NIFs in safe Rust code. That means
there should be no ways to crash the BEAM (Erlang VM). The library provides
facilities for generating the boilerplate for interacting with the BEAM,
handles encoding and decoding of Erlang terms, and catches rust panics before
they unwind into C.

The library provides functionality for both Erlang and Elixir, however Elixir
is favored as of now.

**The current relase on crates.io is very old because we are in the middle of some big changes. If you want to use the project now you should [use git master](https://github.com/hansihe/Rustler/issues/27#issuecomment-261367947), but be prepared for breaking changes.**

#### Features:
* Safety - The code you write in a Rust NIF should never be able to crash the BEAM.
* Interop - Decoding and encoding rust values into Erlang terms is as easy as a function call.
* Type composition - Making a Rust struct encodable and decodable to Erlang or Elixir can be done with a single attribute.
* Resource objects - Enables you to safely pass a reference to a Rust struct into Erlang code. The struct will be automatically dropped when it's no longer referenced.

#### Getting started
The easiest way of getting started is the [rustler elixir library](https://hex.pm/packages/rustler).

* Add the [rustler elixir library](https://hex.pm/packages/rustler) as a dependency of your project.
* Run `mix rustler.new` to generate a new NIF in your project. Follow the instructions.

NOTE: If you have previously used Rustler, you need to run `mix archive.uninstall rustler_installer.ez` to remove it before generating the NIF.

#### How it looks like
This is the code for a minimal NIF that adds two numbers and returns the result.
```rust
#[macro_use] extern crate rustler;
#[macro_use] extern crate lazy_static;

use rustler::{NifEnv, NifTerm, NifResult, NifEncoder};

mod atoms {
    rustler_atoms! {
        atom ok;
    }
}

rustler_export_nifs!(
    "Elixir.TestNifModule",
    [("add", 2, add)],
    None
);

fn add<'a>(env: NifEnv<'a>, args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let num1: i64 = try!(args[0].decode());
    let num2: i64 = try!(args[1].decode());

    Ok((atoms::ok(), num1 + num2).encode(env))
}
```

#### Community

You can find us in `#rustler` on [freenode](http://freenode.net/) or [the elixir-lang slack](https://elixir-slackin.herokuapp.com/).

#### License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

##### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
