//! Utilities used to access and create Erlang maps.

use super::{ NifEnv, NifTerm, NifError, NifResult, NifDecoder };
use ::wrapper::map;

pub fn get_map_value<'a>(term: NifTerm<'a>, key: NifTerm) -> Option<NifTerm<'a>> {
    match ::wrapper::get_map_value(term.env.as_c_arg(), term.as_c_arg(), key.as_c_arg()) {
        Some(value) => Some(NifTerm::new(term.env, value)),
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

pub struct NifMapIterator<'a> {
    env: &'a NifEnv,
    iter: map::MapIterator
}

impl<'a> NifMapIterator<'a> {
    pub fn new(map: NifTerm<'a>) -> Option<NifMapIterator<'a>> {
        unsafe {
            map::map_iterator_create(map.env.as_c_arg(), map.as_c_arg())
        }.map(|iter| NifMapIterator { env: map.env, iter: iter })
    }
}

impl<'a> Drop for NifMapIterator<'a> {
    fn drop(&mut self) {
        unsafe {
            map::map_iterator_destroy(self.env.as_c_arg(), &mut self.iter);
        }
    }
}

impl<'a> Iterator for NifMapIterator<'a> {
    type Item = (NifTerm<'a>, NifTerm<'a>);

    fn next(&mut self) -> Option<(NifTerm<'a>, NifTerm<'a>)> {
        unsafe {
            map::map_iterator_get_pair(self.env.as_c_arg(), &mut self.iter)
        }.map(|(key, value)| {
            unsafe {
                map::map_iterator_next(self.env.as_c_arg(), &mut self.iter);
            }
            (NifTerm::new(self.env, key),
             NifTerm::new(self.env, value))
        })
    }
}

impl<'a> NifDecoder<'a> for NifMapIterator<'a> {
    fn decode(term: NifTerm<'a>) -> NifResult<Self> {
        match NifMapIterator::new(term) {
            Some(iter) => Ok(iter),
            None => Err(NifError::BadArg)
        }
    }
}
