/*!
Low level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).
*/

// Don't throw warnings on NIF naming conventions
#![allow(non_camel_case_types)]

pub mod rustler_sys_api;

pub use crate::rustler_sys_api::*;
