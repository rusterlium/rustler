use super::binary::ErlNifBinary;
use super::nif_interface;
use super::nif_interface::{ERL_NIF_BIN2TERM_SAFE, NIF_ENV, NIF_TERM};
use std::mem;

pub unsafe fn binary_to_term(env: NIF_ENV, data: &[u8], safe: bool) -> Option<(NIF_TERM, usize)> {
    let opts = if safe { ERL_NIF_BIN2TERM_SAFE } else { 0 };

    let mut result: NIF_TERM = mem::uninitialized();
    let read_count =
        nif_interface::enif_binary_to_term(env, data.as_ptr(), data.len(), &mut result, opts);

    if read_count == 0 {
        return None;
    }

    Some((result, read_count))
}

pub unsafe fn term_to_binary(env: NIF_ENV, term: NIF_TERM) -> Option<ErlNifBinary> {
    let mut binary = ErlNifBinary::new_empty();
    let success = nif_interface::enif_term_to_binary(env, term, binary.as_c_arg());

    if success == 0 {
        return None;
    }

    Some(binary)
}
