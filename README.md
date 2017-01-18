# erlang_nif-sys (formerly ruster_unsafe)
[![](http://meritbadge.herokuapp.com/erlang_nif-sys)](https://crates.io/crates/erlang_nif-sys)
[![Build Status](https://travis-ci.org/goertzenator/erlang_nif-sys.svg?branch=master)](https://travis-ci.org/goertzenator/erlang_nif-sys)
[![Build status](https://ci.appveyor.com/api/projects/status/rssa03e29mxou4hv/branch/master?svg=true)](https://ci.appveyor.com/project/goertzenator/erlang-nif-sys/branch/master)

A crate for creating [Erlang NIF modules](http://www.erlang.org/doc/man/erl_nif.html) in Rust.  This crate exposes the raw C NIF API which can be used directly or as a foundation for higher layer interface crates.  Supported under Unix and Windows.

See the [crate documention](http://goertzenator.github.io/erlang_nif-sys/erlang_nif_sys/index.html).

See examples of use:
 - [rust.mk](https://github.com/goertzenator/rust.mk) for a sample Rust NIF module.
 - [rebar3_rust](https://github.com/sdwolf/rebar3_rust) a rebar3 plugin inspired by `rust.mk` that helps integrate Rust code inside Erlang projects.
 - [Rustler](https://github.com/hansihe/Rustler)
 - [rustfromerl](https://github.com/sdwolf/rustfromerl) a demo project showing performance differences between Erlang code and a simmilar Rust NIF implementation.

Thanks go to Radosław Szymczyszyn for bootstrapping me on this Rust FFI adventure and providing the original [automatic bindings](https://github.com/lavrin/erlang-rust-nif/blob/master/rust_src/src/c.rs).
