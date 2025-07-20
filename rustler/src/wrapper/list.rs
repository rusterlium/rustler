use crate::{
    sys::{
        enif_get_list_cell, enif_get_list_length, enif_make_list_cell, enif_make_list_from_array,
        enif_make_reverse_list,
    },
    wrapper::{NIF_ENV, NIF_TERM},
};
use std::mem::MaybeUninit;

#[inline]
pub unsafe fn get_list_cell(env: NIF_ENV, list: NIF_TERM) -> Option<(NIF_TERM, NIF_TERM)> {
    let mut head = MaybeUninit::uninit();
    let mut tail = MaybeUninit::uninit();
    let success = enif_get_list_cell(env, list, head.as_mut_ptr(), tail.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some((head.assume_init(), tail.assume_init()))
}

#[inline]
pub unsafe fn get_list_length(env: NIF_ENV, list: NIF_TERM) -> Option<usize> {
    let mut len: u32 = 0;
    let success = enif_get_list_length(env, list, &mut len);

    if success != 1 {
        return None;
    }
    Some(len as usize)
}

#[inline]
pub unsafe fn make_list(env: NIF_ENV, arr: &[NIF_TERM]) -> NIF_TERM {
    enif_make_list_from_array(env, arr.as_ptr(), arr.len() as u32)
}

#[inline]
pub unsafe fn make_list_cell(env: NIF_ENV, head: NIF_TERM, tail: NIF_TERM) -> NIF_TERM {
    enif_make_list_cell(env, head, tail)
}

#[inline]
pub unsafe fn make_reverse_list(env: NIF_ENV, list: NIF_TERM) -> Option<NIF_TERM> {
    let mut list_out = MaybeUninit::uninit();
    let success = enif_make_reverse_list(env, list, list_out.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(list_out.assume_init())
}
