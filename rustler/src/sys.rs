/*!
Low level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).
*/

// Don't throw warnings on NIF naming conventions
#![allow(non_camel_case_types)]
#![allow(clippy::missing_safety_doc)]

mod functions;
mod nif_filler;
mod types;

pub use self::functions::*;
pub use self::types::*;

pub const NIF_MAJOR_VERSION: c_int = 2;

pub const NIF_MINOR_VERSION: c_int = if cfg!(feature = "nif_version_2_18") {
    18
} else if cfg!(feature = "nif_version_2_17") {
    17
} else if cfg!(feature = "nif_version_2_16") {
    16
} else if cfg!(feature = "nif_version_2_15") {
    15
} else if cfg!(feature = "nif_version_2_14") {
    14
} else {
    unreachable!()
};
