use crate::wrapper::{c_uint, NIF_ENV, NIF_TERM};
use crate::Error;
use erl_nif_sys::ErlNifCharEncoding::ERL_NIF_LATIN1;

pub unsafe fn make_atom(env: NIF_ENV, name: &[u8]) -> NIF_TERM {
    erl_nif_sys::enif_make_atom_len(env, name.as_ptr(), name.len())
}

pub unsafe fn make_existing_atom(env: NIF_ENV, name: &[u8]) -> Option<NIF_TERM> {
    let mut atom_out: NIF_TERM = 0;
    let success = erl_nif_sys::enif_make_existing_atom_len(
        env,
        name.as_ptr(),
        name.len(),
        &mut atom_out as *mut NIF_TERM,
        ERL_NIF_LATIN1,
    );
    if success == 0 {
        return None;
    }
    Some(atom_out)
}

/// Get the contents of this atom as a string.
///
/// If you only need to test for equality, comparing the terms directly
/// is much faster.
///
/// # Errors
///
/// `Error::BadArg` if `term` is not an atom.
///
pub unsafe fn get_atom(env: NIF_ENV, term: NIF_TERM) -> Result<String, Error> {
    // Determine the length of the atom, in bytes.
    let mut len = 0;
    let success = erl_nif_sys::enif_get_atom_length(env, term, &mut len, ERL_NIF_LATIN1);
    if success == 0 {
        return Err(Error::BadArg);
    }

    // Get the bytes from the atom into a buffer.
    // enif_get_atom() writes a null terminated string,
    // so add 1 to the atom's length to make room for it.
    let mut bytes: Vec<u8> = Vec::with_capacity(len as usize + 1);
    let nbytes = erl_nif_sys::enif_get_atom(env, term, bytes.as_mut_ptr(), len + 1, ERL_NIF_LATIN1);
    assert!(nbytes as c_uint == len + 1);

    // This is safe unless the VM is lying to us.
    bytes.set_len(len as usize); // drop the null byte

    // Convert from Latin-1 bytes to a String.
    let nonascii_count = bytes.iter().filter(|&&b| b >= 128).count();
    if nonascii_count == 0 {
        // The string is ASCII, so it is safe to convert without copying.
        Ok(String::from_utf8_unchecked(bytes))
    } else {
        // Transcode from Latin-1 to UTF-8.
        let mut out = String::with_capacity(bytes.len() + nonascii_count);
        for b in bytes {
            out.push(b as char);
        }
        Ok(out)
    }
}
