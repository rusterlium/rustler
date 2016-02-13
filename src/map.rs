//! Utilities used to access and create Erlang maps.

use super::{ NifEnv, NifTerm };
use ::wrapper::map;

pub fn get_map_value<'a>(term: NifTerm<'a>, key: NifTerm) -> Option<NifTerm<'a>> {
    match ::wrapper::get_map_value(term.env.as_c_arg(), term.as_c_arg(), key.as_c_arg()) {
        Some(value) => Some(unsafe { NifTerm::new(term.env, value) }),
        None => None,
    }
}

pub fn map_new<'a>(env: &'a NifEnv) -> NifTerm<'a> {
    NifTerm::new(env, map::map_new(env.as_c_arg()))
}

pub fn map_put<'a>(map: NifTerm<'a>, key: NifTerm, value: NifTerm) -> Option<NifTerm<'a>> {
    assert!(map.env == key.env, "key is from different environment as map");
    assert!(map.env == value.env, "value is from different environment as map");
    match map::map_put(map.env.as_c_arg(), map.as_c_arg(), key.as_c_arg(), value.as_c_arg()) {
        Some(inner) => Some(NifTerm::new(map.env, inner)),
        None => None
    }
}
