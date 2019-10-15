use crate::wrapper::{c_int, NIF_ENV, NIF_ERROR, NIF_TERM};
use std::mem::MaybeUninit;

pub unsafe fn get_tuple<'a>(env: NIF_ENV, term: NIF_TERM) -> Result<&'a [NIF_TERM], NIF_ERROR> {
    let mut arity: c_int = 0;
    let mut array_ptr = MaybeUninit::uninit();
    let success = rustler_sys::enif_get_tuple(env, term, &mut arity, array_ptr.as_mut_ptr());
    if success != 1 {
        return Err(NIF_ERROR::BAD_ARG);
    }
    let term_array = ::std::slice::from_raw_parts(array_ptr.assume_init(), arity as usize);
    Ok(term_array)
}

pub unsafe fn make_tuple(env: NIF_ENV, terms: &[NIF_TERM]) -> NIF_TERM {
    rustler_sys::enif_make_tuple_from_array(env, terms.as_ptr(), terms.len() as u32)
}
