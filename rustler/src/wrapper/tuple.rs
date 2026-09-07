use crate::sys::{c_int, enif_get_tuple, enif_make_tuple_from_array, ErlNifEnv, ErlNifTerm};
use crate::Error;
use std::mem::MaybeUninit;

pub unsafe fn get_tuple<'a>(
    env: *mut ErlNifEnv,
    term: ErlNifTerm,
) -> Result<&'a [ErlNifTerm], Error> {
    let mut arity: c_int = 0;
    let mut array_ptr = MaybeUninit::uninit();
    let success = enif_get_tuple(env, term, &mut arity, array_ptr.as_mut_ptr());
    if success != 1 {
        return Err(Error::BadArg);
    }
    let term_array = ::std::slice::from_raw_parts(array_ptr.assume_init(), arity as usize);
    Ok(term_array)
}

pub unsafe fn make_tuple(env: *mut ErlNifEnv, terms: &[ErlNifTerm]) -> ErlNifTerm {
    enif_make_tuple_from_array(env, terms.as_ptr(), terms.len() as u32)
}
