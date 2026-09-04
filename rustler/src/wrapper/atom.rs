use std::mem::MaybeUninit;

#[cfg(not(feature = "nif_version_2_17"))]
use crate::sys::enif_make_atom_len;
#[cfg(feature = "nif_version_2_17")]
use crate::sys::enif_make_new_atom_len;
use crate::sys::{
    c_char, c_uint, enif_get_atom, enif_get_atom_length, enif_make_existing_atom_len,
    ErlNifCharEncoding, ErlNifEnv, ERL_NIF_TERM,
};
use crate::Error;

#[cfg(not(feature = "nif_version_2_17"))]
pub unsafe fn make_atom(
    env: *mut ErlNifEnv,
    name: &[u8],
    _encoding: ErlNifCharEncoding,
) -> Result<ERL_NIF_TERM, Error> {
    let res = enif_make_atom_len(env, name.as_ptr() as *const c_char, name.len());
    if res.0 != 0 {
        Ok(res)
    } else {
        Err(Error::BadArg)
    }
}

#[cfg(feature = "nif_version_2_17")]
pub unsafe fn make_atom(
    env: *mut ErlNifEnv,
    name: &[u8],
    encoding: ErlNifCharEncoding,
) -> Result<ERL_NIF_TERM, Error> {
    let mut atom_out = MaybeUninit::uninit();

    // Create a new atom with the requested encoding.
    // Returns 0 if creation fails (e.g. invalid text/encoding).
    if enif_make_new_atom_len(
        env,
        name.as_ptr() as *const c_char,
        name.len(),
        atom_out.as_mut_ptr(),
        encoding,
    ) != 0
    {
        Ok(atom_out.assume_init())
    } else {
        Err(Error::BadArg)
    }
}

pub unsafe fn make_existing_atom(
    env: *mut ErlNifEnv,
    name: &[u8],
    encoding: ErlNifCharEncoding,
) -> Result<ERL_NIF_TERM, Error> {
    let mut atom_out = MaybeUninit::uninit();

    if enif_make_existing_atom_len(
        env,
        name.as_ptr() as *const c_char,
        name.len(),
        atom_out.as_mut_ptr(),
        encoding,
    ) != 0
    {
        Ok(atom_out.assume_init())
    } else {
        Err(Error::BadArg)
    }
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
#[cfg(feature = "nif_version_2_17")]
pub unsafe fn get_atom(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> Result<String, Error> {
    // Determine the length of the atom, in bytes.
    let mut len = 0;
    let success = enif_get_atom_length(env, term, &mut len, ErlNifCharEncoding::ERL_NIF_UTF8);
    if success == 0 {
        return Err(Error::BadArg);
    }

    // Get the bytes from the atom into a buffer.
    // enif_get_atom() writes a null terminated string,
    // so add 1 to the atom's length to make room for it.
    let mut string = String::with_capacity(len as usize + 1);
    let bytes = string.as_mut_vec();
    let nbytes = enif_get_atom(
        env,
        term,
        bytes.as_mut_ptr() as *mut c_char,
        len + 1,
        ErlNifCharEncoding::ERL_NIF_UTF8,
    );
    assert!(nbytes as c_uint == len + 1);

    // This relies on Erlang guaranteeing valid UTF-8 for ERL_NIF_UTF8 reads.
    bytes.set_len(len as usize); // drop the null byte

    Ok(string)
}

#[cfg(not(feature = "nif_version_2_17"))]
pub unsafe fn get_atom(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> Result<String, Error> {
    // Determine the length of the atom, in bytes.
    let mut len = 0;
    let success = enif_get_atom_length(env, term, &mut len, ErlNifCharEncoding::ERL_NIF_LATIN1);
    if success == 0 {
        return Err(Error::BadArg);
    }

    // Get the bytes from the atom into a buffer.
    // enif_get_atom() writes a null terminated string,
    // so add 1 to the atom's length to make room for it.
    let mut bytes: Vec<u8> = Vec::with_capacity(len as usize + 1);
    let nbytes = enif_get_atom(
        env,
        term,
        bytes.as_mut_ptr() as *mut c_char,
        len + 1,
        ErlNifCharEncoding::ERL_NIF_LATIN1,
    );
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
