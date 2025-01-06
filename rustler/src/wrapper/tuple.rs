use crate::sys::enif_make_tuple_from_array;
use crate::wrapper::{NIF_ENV, NIF_TERM};

pub unsafe fn make_tuple(env: NIF_ENV, terms: &[NIF_TERM]) -> NIF_TERM {
    enif_make_tuple_from_array(env, terms.as_ptr(), terms.len() as u32)
}
