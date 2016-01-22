//! Utilities used to access and create Erlang maps.

use super::{ NifEnv, NifTerm };
use ::wrapper::map;

pub fn get_map_value<'a>(env: &NifEnv, term: NifTerm<'a>, key: NifTerm) -> Option<NifTerm<'a>> {
    match ::wrapper::get_map_value(env.as_c_arg(), term.as_c_arg(), key.as_c_arg()) {
        Some(value) => Some(unsafe { NifTerm::new_raw(value) }),
        None => None,
    }
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
