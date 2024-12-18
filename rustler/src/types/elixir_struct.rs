//! Utilities used to create and access data specific to Elixir structs. Keep in mind that an
//! Elixir struct is a normal Erlang map, and functions from the `map` module can be used.
//!
//! # Elixir struct transcoders
//! The compiler plugin has functionality for automatically generating a transcoder that can decode
//! and encode a Rust struct to an Elixir struct. To do so, simply annotate a struct with
//! `#[derive(NifStruct)]
//! `#[module = "Elixir.TheStructModule"]`.

use super::atom::{self, Atom};
use super::map::Map;
use crate::{Env, Error, NifResult, Term};

pub fn get_ex_struct_name(map: Term) -> NifResult<Atom> {
    // In an Elixir struct the value in the __struct__ field is always an atom.
    let map: Map<'_> = map.try_into()?;
    map.get(atom::__struct__())
        .ok_or(Error::BadArg)
        .and_then(Atom::from_term)
}

pub fn make_ex_struct<'a>(env: Env<'a>, struct_module: &str) -> NifResult<Map<'a>> {
    let map = env.new_map();

    let struct_atom = atom::__struct__();
    let module_atom = Atom::from_str(env, struct_module)?;

    map.put(struct_atom, module_atom)
}
