//! Utilities used to create and access data specific to Elixir structs. Keep in mind that an
//! Elixir struct is a normal Erlang map, and functions from the `map` module can be used.
//!
//! # Elixir struct transcoders
//! The compiler plugin has functionality for automatically generating a transcoder that can decode
//! and encode a Rust struct to an Elixir struct. To do so, simply annotate a struct with
//! `#[ExStruct(module = "Elixir.TheStructModule")]`.

use ::{ Env, Term, NifResult };
use super::atom::{ self, NifAtom };
use super::map::{ map_new };

pub fn get_ex_struct_name(map: Term) -> NifResult<NifAtom> {
    let env = map.get_env();
    // In an Elixir struct the value in the __struct__ field is always an atom.
    map.map_get(atom::__struct__().to_term(env))
        .and_then(|e| NifAtom::from_term(e))
}

pub fn make_ex_struct<'a>(env: Env<'a>, struct_module: &str) -> NifResult<Term<'a>> {
    let map = map_new(env);

    let struct_atom = atom::__struct__().to_term(env);
    let module_atom = NifAtom::from_str(env, struct_module)?.to_term(env);

    map.map_put(struct_atom, module_atom)
}
