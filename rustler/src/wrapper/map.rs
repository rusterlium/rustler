pub use crate::wrapper::ErlNifMapIterator;
use crate::{
    sys::{
        enif_get_map_size, enif_get_map_value, enif_make_map_from_arrays, enif_make_map_put,
        enif_make_map_remove, enif_make_map_update, enif_make_new_map, enif_map_iterator_prev,
    },
    wrapper::{ErlNifMapIteratorEntry, NIF_ENV, NIF_TERM},
};
use std::mem::MaybeUninit;

use super::{
    enif_map_iterator_create, enif_map_iterator_destroy, enif_map_iterator_get_pair,
    enif_map_iterator_next,
};

pub unsafe fn get_map_value(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM) -> Option<NIF_TERM> {
    let mut result = MaybeUninit::uninit();
    let success = enif_get_map_value(env, map, key, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

pub unsafe fn get_map_size(env: NIF_ENV, map: NIF_TERM) -> Option<usize> {
    let mut size = MaybeUninit::uninit();
    let success = enif_get_map_size(env, map, size.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(size.assume_init())
}

pub unsafe fn map_new(env: NIF_ENV) -> NIF_TERM {
    enif_make_new_map(env)
}

pub unsafe fn map_put(
    env: NIF_ENV,
    map: NIF_TERM,
    key: NIF_TERM,
    value: NIF_TERM,
) -> Option<NIF_TERM> {
    let mut result = MaybeUninit::uninit();
    let success = enif_make_map_put(env, map, key, value, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

pub unsafe fn map_remove(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM) -> Option<NIF_TERM> {
    let mut result = MaybeUninit::uninit();
    let success = enif_make_map_remove(env, map, key, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

pub unsafe fn map_update(
    env: NIF_ENV,
    map: NIF_TERM,
    key: NIF_TERM,
    new_value: NIF_TERM,
) -> Option<NIF_TERM> {
    let mut result = MaybeUninit::uninit();
    let success = enif_make_map_update(env, map, key, new_value, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

#[derive(Clone, Copy, Debug)]
pub enum MapIteratorEntry {
    First,
    Last,
}

pub unsafe fn map_iterator_create(
    env: NIF_ENV,
    map: NIF_TERM,
    entry: MapIteratorEntry,
) -> Option<ErlNifMapIterator> {
    let mut iter = MaybeUninit::uninit();
    let success = enif_map_iterator_create(
        env,
        map,
        iter.as_mut_ptr(),
        match entry {
            MapIteratorEntry::First => ErlNifMapIteratorEntry::ERL_NIF_MAP_ITERATOR_HEAD,
            MapIteratorEntry::Last => ErlNifMapIteratorEntry::ERL_NIF_MAP_ITERATOR_TAIL,
        },
    );
    if success == 0 {
        None
    } else {
        Some(iter.assume_init())
    }
}

pub unsafe fn map_iterator_destroy(env: NIF_ENV, iter: &mut ErlNifMapIterator) {
    enif_map_iterator_destroy(env, iter);
}

pub unsafe fn map_iterator_get_pair(
    env: NIF_ENV,
    iter: &mut ErlNifMapIterator,
) -> Option<(NIF_TERM, NIF_TERM)> {
    let mut key = MaybeUninit::uninit();
    let mut value = MaybeUninit::uninit();
    if enif_map_iterator_get_pair(env, iter, key.as_mut_ptr(), value.as_mut_ptr()) == 0 {
        None
    } else {
        Some((key.assume_init(), value.assume_init()))
    }
}

#[inline]
pub unsafe fn map_iterator_next(env: NIF_ENV, iter: &mut ErlNifMapIterator) {
    enif_map_iterator_next(env, iter);
}

pub unsafe fn map_iterator_prev(env: NIF_ENV, iter: &mut ErlNifMapIterator) {
    enif_map_iterator_prev(env, iter);
}

#[inline]
pub unsafe fn make_map_from_arrays(
    env: NIF_ENV,
    keys: &[NIF_TERM],
    values: &[NIF_TERM],
) -> Option<NIF_TERM> {
    let mut map = MaybeUninit::uninit();
    if enif_make_map_from_arrays(
        env,
        keys.as_ptr(),
        values.as_ptr(),
        keys.len(),
        map.as_mut_ptr(),
    ) == 0
    {
        return None;
    }

    Some(map.assume_init())
}
