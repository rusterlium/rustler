use crate::sys::{
    enif_binary_to_term, enif_term_to_binary, ErlNifBinary, ErlNifEnv, ERL_NIF_BIN2TERM_SAFE,
    ERL_NIF_TERM,
};
use std::mem::MaybeUninit;

pub unsafe fn binary_to_term(
    env: *mut ErlNifEnv,
    data: &[u8],
    safe: bool,
) -> Option<(ERL_NIF_TERM, usize)> {
    let opts = if safe { ERL_NIF_BIN2TERM_SAFE } else { 0 };

    let mut result = MaybeUninit::uninit();
    let read_count = enif_binary_to_term(env, data.as_ptr(), data.len(), result.as_mut_ptr(), opts);

    if read_count == 0 {
        return None;
    }

    Some((result.assume_init(), read_count))
}

pub unsafe fn term_to_binary(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> Option<ErlNifBinary> {
    let mut binary = MaybeUninit::uninit();
    let success = enif_term_to_binary(env, term, binary.as_mut_ptr());

    if success == 0 {
        return None;
    }

    Some(binary.assume_init())
}
