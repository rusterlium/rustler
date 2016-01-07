use super::nif_interface;
use super::nif_interface::{ NIF_ENV, NIF_TERM };
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
    Some(size)
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
