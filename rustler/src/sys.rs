/*!
Low level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).
*/

// Don't throw warnings on NIF naming conventions
#![allow(non_camel_case_types)]
#![allow(clippy::missing_safety_doc)]

#[macro_use]
mod macros;
mod functions;
mod nif_filler;
mod types;

pub use self::functions::*;
pub use self::types::*;
pub use crate::{enif_fprintf, enif_make_list, enif_make_tuple, enif_snprintf};
