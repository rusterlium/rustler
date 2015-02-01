# Ruster
A library for creating Erlang NIFs in Rust

## Status

Not yet working.

I'm very close to compiling a valid immutable static ErlNifEntry, but am currently blocked by an internal compiler error.  After that is fixed, some tidy-up, macrofication, and documentation should yield a useable Rust binding.


## General Design Notes
A quick note to explain what is going on here:

- An Erlang script generates NIF API function signatures for Rust.  This roughly replicates the metaprogramming that goes on in erl_nif.h
- NIF structs and datatypes are handwritten in Rust (well, actually pilfered and cleaned up from automatic bindings provided by Radosław Szymczyszyn.)
- There will be a different Ruster branch for each supported version of Erlang.  Just like erl_nif.h.
- I've left the door open for Windows support, although this will not be the initial focus.
- Since Rust has a very nice threading API, I've completely skipping the NIF threading API.  It could be added later if needed.
- I would like to have a low level (unsafe) API that directly maps to the C NIF API, and a higher level API that follows Rust idioms.  I intend the higher level API to be zero cost or nearly zero cost.
