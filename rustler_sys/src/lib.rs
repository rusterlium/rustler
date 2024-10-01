/*!
Low level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).
*/

// Don't throw warnings on NIF naming conventions
#![allow(non_camel_case_types)]
#![allow(clippy::missing_safety_doc)]

mod functions;
mod nif_filler;
mod types;

pub use crate::functions::*;
pub use crate::types::*;
