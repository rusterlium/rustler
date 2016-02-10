# Rustler

[Documentation](http://rustler.rustbridge.io/) | [Getting Started](http://rustler.rustbridge.io/) | [Example](https://github.com/hansihe/Rustler_Example)

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
