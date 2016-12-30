use super::nif_interface;
use super::nif_interface::{ NIF_ENV, NIF_TERM, ErlNifMapIteratorEntry };
use std::mem;

pub fn get_map_value(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM) -> Option<NIF_TERM> {
    let mut result: NIF_TERM = unsafe { mem::uninitialized() };
    let success = unsafe { nif_interface::enif_get_map_value(env, map, key, &mut result as *mut NIF_TERM) };

    if success != 1 {
        return None;
    }
    Some(result)
}

pub fn get_map_size(env: NIF_ENV, map: NIF_TERM) -> Option<usize> {
    let mut size: nif_interface::size_t = unsafe { mem::uninitialized() };
    let success = unsafe { nif_interface::enif_get_map_size(env, map, &mut size as *mut nif_interface::size_t) };

    if success != 1 {
        return None;
    }
    Some(size as usize)
}

pub fn map_new(env: NIF_ENV) -> NIF_TERM {
    unsafe { nif_interface::enif_make_new_map(env) }
}

pub fn map_put(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM, value: NIF_TERM) -> Option<NIF_TERM> {
    let mut result: NIF_TERM = unsafe { mem::uninitialized() };
    let success = unsafe { nif_interface::enif_make_map_put(env, map, key, value, &mut result as *mut NIF_TERM) };

    if success != 1 {
        return None;
    }
    Some(result)
}

pub struct MapIterator {
    iter: nif_interface::ErlNifMapIterator
}

pub unsafe fn map_iterator_create(env: NIF_ENV, map: NIF_TERM) -> Option<MapIterator> {
    let mut iter = MapIterator {
        iter: mem::uninitialized()
    };

    let success =
        nif_interface::enif_map_iterator_create(env, map,
                                                &mut iter.iter,
                                                ErlNifMapIteratorEntry::ERL_NIF_MAP_ITERATOR_HEAD);
    if success == 0 {
        None
    } else {
        Some(iter)
    }
}

pub unsafe fn map_iterator_destroy(env: NIF_ENV, iter: &mut MapIterator) {
    nif_interface::enif_map_iterator_destroy(env, &mut iter.iter);
}

pub unsafe fn map_iterator_get_pair(env: NIF_ENV, iter: &mut MapIterator) -> Option<(NIF_TERM, NIF_TERM)> {
    let mut key: NIF_TERM = mem::uninitialized();
    let mut value: NIF_TERM = mem::uninitialized();
    if nif_interface::enif_map_iterator_get_pair(env, &mut iter.iter, &mut key, &mut value) == 0 {
        None
    } else {
        Some((key, value))
    }
}

pub unsafe fn map_iterator_next(env: NIF_ENV, iter: &mut MapIterator) {
    nif_interface::enif_map_iterator_next(env, &mut iter.iter);
}
