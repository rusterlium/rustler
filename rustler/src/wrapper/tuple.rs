use super::nif_interface;
use super::nif_interface::{c_int, NIF_ENV, NIF_ERROR, NIF_TERM};
use std::mem;

pub unsafe fn get_tuple<'a>(env: NIF_ENV, term: NIF_TERM) -> Result<&'a [NIF_TERM], NIF_ERROR> {
    let mut arity: c_int = 0;
    let mut array_ptr: *const NIF_TERM = mem::uninitialized();
    let success = nif_interface::enif_get_tuple(env, term, &mut arity, &mut array_ptr);
    if success != 1 {
        return Err(NIF_ERROR::BAD_ARG);
    }
    let term_array = ::std::slice::from_raw_parts(array_ptr, arity as usize);
    Ok(term_array)
}

pub unsafe fn make_tuple(env: NIF_ENV, terms: &[NIF_TERM]) -> NIF_TERM {
    nif_interface::enif_make_tuple_from_array(env, terms.as_ptr(), terms.len() as u32)
}
