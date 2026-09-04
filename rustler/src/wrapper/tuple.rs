use crate::sys::{c_int, enif_get_tuple, enif_make_tuple_from_array, ErlNifEnv, ERL_NIF_TERM};
use crate::wrapper::NIF_ERROR;
use std::mem::MaybeUninit;

pub unsafe fn get_tuple<'a>(
    env: *mut ErlNifEnv,
    term: ERL_NIF_TERM,
) -> Result<&'a [ERL_NIF_TERM], NIF_ERROR> {
    let mut arity: c_int = 0;
    let mut array_ptr = MaybeUninit::uninit();
    let success = enif_get_tuple(env, term, &mut arity, array_ptr.as_mut_ptr());
    if success != 1 {
        return Err(NIF_ERROR::BAD_ARG);
    }
    let term_array = ::std::slice::from_raw_parts(array_ptr.assume_init(), arity as usize);
    Ok(term_array)
}

pub unsafe fn make_tuple(env: *mut ErlNifEnv, terms: &[ERL_NIF_TERM]) -> ERL_NIF_TERM {
    enif_make_tuple_from_array(env, terms.as_ptr(), terms.len() as u32)
}
