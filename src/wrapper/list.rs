use super::nif_interface;
use super::nif_interface::{ NIF_ENV, NIF_TERM };
use std::mem;

pub fn get_list_cell(env: NIF_ENV, list: NIF_TERM) -> Option<(NIF_TERM, NIF_TERM)> {
    let mut head: NIF_TERM = unsafe { mem::uninitialized() };
    let mut tail: NIF_TERM = unsafe { mem::uninitialized() };
    let success = unsafe { nif_interface::enif_get_list_cell(env, list, &mut head as *mut NIF_TERM, &mut tail as *mut NIF_TERM) };

    if success != 1 {
        return None;
    }
    Some((head, tail))
}

pub fn get_list_length(env: NIF_ENV, list: NIF_TERM) -> Option<usize> {
    let mut len: u32 = 0;
    let success = unsafe { nif_interface::enif_get_list_length(env, list, &mut len as *mut u32) };

    if success != 1 {
        return None;
    }
    Some(len as usize)
}

pub fn make_list(env: NIF_ENV, arr: &[NIF_TERM]) -> NIF_TERM {
    // FIXME?: Should we downcast like this?
    unsafe { nif_interface::enif_make_list_from_array(env, arr.as_ptr(), arr.len() as u32) }
}

pub fn make_list_cell(env: NIF_ENV, head: NIF_TERM, tail: NIF_TERM) -> NIF_TERM {
    unsafe { nif_interface::enif_make_list_cell(env, head, tail) }
}

pub fn make_reverse_list(env: NIF_ENV, list: NIF_TERM) -> Option<NIF_TERM> {
    let mut list_out: NIF_TERM = unsafe { mem::uninitialized() };
    let success = unsafe { nif_interface::enif_make_reverse_list(env, list, &mut list_out as *mut NIF_TERM) };

    if success != 1 {
        return None;
    }
    Some(list_out)
}
