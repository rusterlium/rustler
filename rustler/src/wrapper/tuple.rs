use crate::sys::enif_make_tuple_from_array;
use crate::wrapper::{c_int, NIF_ENV, NIF_ERROR, NIF_TERM};
use std::mem::MaybeUninit;

pub unsafe fn make_tuple(env: NIF_ENV, terms: &[NIF_TERM]) -> NIF_TERM {
    enif_make_tuple_from_array(env, terms.as_ptr(), terms.len() as u32)
}
