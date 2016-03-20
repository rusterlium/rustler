# Ruster (unsafe)
[![](http://meritbadge.herokuapp.com/ruster_unsafe)](https://crates.io/crates/ruster_unsafe)

A crate for creating [Erlang NIF modules](http://www.erlang.org/doc/man/erl_nif.html) in Rust.  This crate exposes the raw C NIF API which can be used directly or as a foundation for higher layer interface crates.  Supported under Unix and Windows.

See the [crate documention](http://goertzenator.github.io/ruster_unsafe/ruster_unsafe/index.html).

See [ruster_unsafe_demo](https://github.com/goertzenator/ruster_unsafe_demo).

Thanks go to Radosław Szymczyszyn for bootstrapping me on this Rust FFI adventure and providing the original [automatic bindings](https://github.com/lavrin/erlang-rust-nif/blob/master/rust_src/src/c.rs).
