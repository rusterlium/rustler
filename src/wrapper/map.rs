use super::nif_interface;
use super::nif_interface::{ NIF_ENV, NIF_TERM, ErlNifMapIteratorEntry };
pub use super::nif_interface::ErlNifMapIterator;
use std::mem;

pub unsafe fn get_map_value(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM) -> Option<NIF_TERM> {
    let mut result: NIF_TERM = mem::uninitialized();
    let success = nif_interface::enif_get_map_value(env, map, key, &mut result as *mut NIF_TERM);

    if success != 1 {
        return None;
    }
    Some(result)
}

pub unsafe fn get_map_size(env: NIF_ENV, map: NIF_TERM) -> Option<usize> {
    let mut size: nif_interface::size_t = mem::uninitialized();
    let success = nif_interface::enif_get_map_size(env, map, &mut size as *mut nif_interface::size_t);

    if success != 1 {
        return None;
    }
    Some(size as usize)
}

pub unsafe fn map_new(env: NIF_ENV) -> NIF_TERM {
    nif_interface::enif_make_new_map(env)
}

pub unsafe fn map_put(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM, value: NIF_TERM) -> Option<NIF_TERM> {
    let mut result: NIF_TERM = mem::uninitialized();
    let success = nif_interface::enif_make_map_put(env, map, key, value, &mut result as *mut NIF_TERM);

    if success != 1 {
        return None;
    }
    Some(result)
}

pub unsafe fn map_remove(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM) -> Option<NIF_TERM> {
    let mut result: NIF_TERM = mem::uninitialized();
    let success = nif_interface::enif_make_map_remove(env, map, key, &mut result as *mut NIF_TERM);

    if success != 1 {
        return None;
    }
    Some(result)
}

pub unsafe fn map_update(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM, new_value: NIF_TERM) -> Option<NIF_TERM> {
    let mut result: NIF_TERM = mem::uninitialized();
    let success = nif_interface::enif_make_map_update(env, map, key, new_value, &mut result as *mut NIF_TERM);

    if success != 1 {
        return None;
    }
    Some(result)
}

pub unsafe fn map_iterator_create(env: NIF_ENV, map: NIF_TERM) -> Option<ErlNifMapIterator> {
    let mut iter = mem::uninitialized();
    let success =
        nif_interface::enif_map_iterator_create(env, map, &mut iter,
                                                ErlNifMapIteratorEntry::ERL_NIF_MAP_ITERATOR_HEAD);
    if success == 0 {
        None
    } else {
        Some(iter)
    }
}

pub unsafe fn map_iterator_destroy(env: NIF_ENV, iter: &mut ErlNifMapIterator) {
    nif_interface::enif_map_iterator_destroy(env, iter);
}

pub unsafe fn map_iterator_get_pair(env: NIF_ENV, iter: &mut ErlNifMapIterator) -> Option<(NIF_TERM, NIF_TERM)> {
    let mut key: NIF_TERM = mem::uninitialized();
    let mut value: NIF_TERM = mem::uninitialized();
    if nif_interface::enif_map_iterator_get_pair(env, iter, &mut key, &mut value) == 0 {
        None
    } else {
        Some((key, value))
    }
}

pub unsafe fn map_iterator_next(env: NIF_ENV, iter: &mut ErlNifMapIterator) {
    nif_interface::enif_map_iterator_next(env, iter);
}
