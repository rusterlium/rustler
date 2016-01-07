use super::{ NifEnv, NifTerm };
use ::atom::NifAtom;
use ::wrapper::map;

pub fn get_map_value<'a>(env: &'a NifEnv, term: NifTerm, key: NifTerm) -> Option<NifTerm<'a>> {
    match ::wrapper::get_map_value(env.as_c_arg(), term.as_c_arg(), key.as_c_arg()) {
        Some(value) => Some(NifTerm::new(env, value)),
        None => None,
    }
}

pub fn get_ex_struct_name(env: &NifEnv, map: NifTerm) -> Option<NifAtom> {
    // In an Elixir struct the value in the __STRUCT__ field is always an atom.
    match get_map_value(env, map, ::atom::get_atom_init("__struct__").to_term(env)) {
        Some(term) => NifAtom::from_term(env, term),
        None => None
    }
}

pub fn make_ex_struct<'a>(env: &'a NifEnv, struct_module: &'static str) -> Option<NifTerm<'a>> {
    let map = map_new(env);
    map_put(env, map, ::atom::get_atom_init("__struct__").to_term(env), ::atom::get_atom_init(struct_module).to_term(env))
}

pub fn map_new<'a>(env: &'a NifEnv) -> NifTerm<'a> {
    NifTerm::new(env, map::map_new(env.as_c_arg()))
}

pub fn map_put<'a>(env: &'a NifEnv, map: NifTerm, key: NifTerm, value: NifTerm) -> Option<NifTerm<'a>> {
    match map::map_put(env.as_c_arg(), map.as_c_arg(), key.as_c_arg(), value.as_c_arg()) {
        Some(inner) => Some(NifTerm::new(env, inner)),
        None => None
    }
}
