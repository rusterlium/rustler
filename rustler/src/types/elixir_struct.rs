//! Utilities used to create and access data specific to Elixir structs. Keep in mind that an
//! Elixir struct is a normal Erlang map, and functions from the `map` module can be used.
//!
//! # Elixir struct transcoders
//! The compiler plugin has functionality for automatically generating a transcoder that can decode
//! and encode a Rust struct to an Elixir struct. To do so, simply annotate a struct with
//! `#[ExStruct(module = "Elixir.TheStructModule")]`.

use super::atom::{self, Atom};
use super::map::map_new;
use crate::{Env, NifResult, Term};

pub fn get_ex_struct_name(map: Term) -> NifResult<Atom> {
    let env = map.get_env();
    // In an Elixir struct the value in the __struct__ field is always an atom.
    map.map_get(atom::__struct__().to_term(env))
        .and_then(Atom::from_term)
}

pub fn make_ex_struct<'a>(env: Env<'a>, struct_module: &str) -> NifResult<Term<'a>> {
    let map = map_new(env);

    let struct_atom = atom::__struct__().to_term(env);
    let module_atom = Atom::from_str(env, struct_module)?.to_term(env);

    map.map_put(struct_atom, module_atom)
}
