use super::nif_interface;
use super::{ NIF_ENV, NIF_TERM };

pub fn make_atom(env: NIF_ENV, name: &str) -> NIF_TERM {
    unsafe { nif_interface::enif_make_atom_len(env, name.as_ptr() as *const u8, name.len() as usize) }
}

pub fn get_atom(env: NIF_ENV, term: NIF_TERM) -> Option<String> {
    // Max 257 characters can be written (256 + NULL).
    let mut string = String::with_capacity(257);

    {
        // We will validate UTF8 at the end of the function.
        let mut string_buf = unsafe { string.as_mut_vec() };

        let result = unsafe { nif_interface::enif_get_atom_latin1(env, term, string_buf.as_mut_ptr(), 257) };

        if result == 0 {
            return None;
        }

        // This SHOULD be safe unless the VM is lying to us.
        unsafe { string_buf.set_len((result - 1) as usize) };
    }

    // REQUIRED! This will verify that the data is actually valid
    // unicode without copying.
    match ::std::str::from_utf8(string.as_bytes()) {
        Ok(_) => Some(string),
        Err(_) => None,
    }
}
