pub use crate::wrapper::ErlNifMapIterator;
use crate::wrapper::{ErlNifMapIteratorEntry, NIF_ENV, NIF_TERM};
use std::mem::MaybeUninit;

pub unsafe fn get_map_value(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM) -> Option<NIF_TERM> {
    let mut result = MaybeUninit::uninit();
    let success = rustler_sys::enif_get_map_value(env, map, key, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

pub unsafe fn get_map_size(env: NIF_ENV, map: NIF_TERM) -> Option<usize> {
    let mut size = MaybeUninit::uninit();
    let success = rustler_sys::enif_get_map_size(env, map, size.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(size.assume_init())
}

pub unsafe fn map_new(env: NIF_ENV) -> NIF_TERM {
    rustler_sys::enif_make_new_map(env)
}

pub unsafe fn map_put(
    env: NIF_ENV,
    map: NIF_TERM,
    key: NIF_TERM,
    value: NIF_TERM,
) -> Option<NIF_TERM> {
    let mut result = MaybeUninit::uninit();
    let success = rustler_sys::enif_make_map_put(env, map, key, value, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

pub unsafe fn map_remove(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM) -> Option<NIF_TERM> {
    let mut result = MaybeUninit::uninit();
    let success = rustler_sys::enif_make_map_remove(env, map, key, result.as_mut_ptr());

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
    let success = rustler_sys::enif_make_map_update(env, map, key, new_value, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

pub unsafe fn map_iterator_create(env: NIF_ENV, map: NIF_TERM) -> Option<ErlNifMapIterator> {
    let mut iter = MaybeUninit::uninit();
    let success = rustler_sys::enif_map_iterator_create(
        env,
        map,
        iter.as_mut_ptr(),
        ErlNifMapIteratorEntry::ERL_NIF_MAP_ITERATOR_HEAD,
    );
    if success == 0 {
        None
    } else {
        Some(iter.assume_init())
    }
}

pub unsafe fn map_iterator_destroy(env: NIF_ENV, iter: &mut ErlNifMapIterator) {
    rustler_sys::enif_map_iterator_destroy(env, iter);
}

pub unsafe fn map_iterator_get_pair(
    env: NIF_ENV,
    iter: &mut ErlNifMapIterator,
) -> Option<(NIF_TERM, NIF_TERM)> {
    let mut key = MaybeUninit::uninit();
    let mut value = MaybeUninit::uninit();
    if rustler_sys::enif_map_iterator_get_pair(env, iter, key.as_mut_ptr(), value.as_mut_ptr()) == 0
    {
        None
    } else {
        Some((key.assume_init(), value.assume_init()))
    }
}

pub unsafe fn map_iterator_next(env: NIF_ENV, iter: &mut ErlNifMapIterator) {
    rustler_sys::enif_map_iterator_next(env, iter);
}

#[cfg(nif_version_2_14)]
pub unsafe fn make_map_from_arrays(
    env: NIF_ENV,
    keys: &[NIF_TERM],
    values: &[NIF_TERM],
) -> Option<NIF_TERM> {
    let mut map = MaybeUninit::uninit();
    if rustler_sys::enif_make_map_from_arrays(
        env,
        keys.as_ptr(),
        values.as_ptr(),
        keys.len() as usize,
        map.as_mut_ptr(),
    ) == 0
    {
        return None;
    }

    Some(map.assume_init())
}
