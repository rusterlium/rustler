//! Utilities used to create and access data spesific to Elixir structs. Keep in mind that an
//! Elixir struct is a normal Erlang map, and functions from the `map` module can be used.
//! 
//! # Elixir struct transcoders
//! The compiler plugin has functionality for automatically generating a transcoder that can decode
//! and encode a Rust struct to an Elixir struct. To do so, simply annotate a struct with
//! `#[ExStruct(module = "Elixir.TheStructModule")]`.

use super::{ NifEnv, NifTerm };
use ::atom::NifAtom;
use ::map::{ map_new };

pub fn get_ex_struct_name(map: NifTerm) -> Option<NifAtom> {
    let env = map.get_env();
    // In an Elixir struct the value in the __struct__ field is always an atom.
    match map.map_get(::atom::get_atom_init("__struct__").to_term(env)) {
        Some(term) => NifAtom::from_term(env, term),
        None => None
    }
}

pub fn make_ex_struct<'a>(env: &'a NifEnv, struct_module: &'static str) -> Option<NifTerm<'a>> {
    let map = map_new(env);
    map.map_put(::atom::get_atom_init("__struct__").to_term(env), ::atom::get_atom_init(struct_module).to_term(env))
}
