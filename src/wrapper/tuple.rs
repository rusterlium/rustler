use super::nif_interface;
use super::nif_interface::{ c_int, NIF_ENV, NIF_TERM, NIF_ERROR };
use std::mem;

pub fn get_tuple<'env_life>(env: NIF_ENV, term: NIF_TERM) -> Result<&'env_life [NIF_TERM], NIF_ERROR> {
    let mut arity: c_int = 0;
    let mut array_ptr: *const NIF_TERM = unsafe { mem::uninitialized() };
    let success = unsafe { nif_interface::enif_get_tuple(env, term,
                                                &mut arity as *mut c_int,
                                                &mut array_ptr as *mut *const NIF_TERM) };
    if success != 1 {
        return Err(NIF_ERROR::BAD_ARG);
    }
    let term_array = unsafe { ::std::slice::from_raw_parts(array_ptr, arity as usize) };
    Ok(term_array)
}
