//! Environment-independent atom terms.

use crate::sys::{c_char, c_uint, enif_get_atom, enif_get_atom_length, ErlNifCharEncoding};
use crate::{Env, Error, NifResult};

free_term!(Atom, crate::Term::is_atom);

impl Atom {
    /// Get the atom's string representation using any environment.
    pub fn to_string(self, env: Env<'_>) -> NifResult<String> {
        #[cfg(feature = "nif_version_2_17")]
        let encoding = ErlNifCharEncoding::ERL_NIF_UTF8;
        #[cfg(not(feature = "nif_version_2_17"))]
        let encoding = ErlNifCharEncoding::ERL_NIF_LATIN1;

        let mut len = 0;
        if unsafe { enif_get_atom_length(env.as_c_arg(), self.as_c_arg(), &mut len, encoding) } == 0
        {
            return Err(Error::BadArg);
        }
        let mut bytes: Vec<u8> = Vec::with_capacity(len as usize + 1);
        unsafe {
            let nbytes = enif_get_atom(
                env.as_c_arg(),
                self.as_c_arg(),
                bytes.as_mut_ptr() as *mut c_char,
                len + 1,
                encoding,
            );
            assert!(nbytes as c_uint == len + 1);
            bytes.set_len(len as usize);
        }
        #[cfg(feature = "nif_version_2_17")]
        return Ok(unsafe { String::from_utf8_unchecked(bytes) });
        #[cfg(not(feature = "nif_version_2_17"))]
        {
            let nonascii_count = bytes.iter().filter(|&&b| b >= 128).count();
            if nonascii_count == 0 {
                Ok(unsafe { String::from_utf8_unchecked(bytes) })
            } else {
                let mut out = String::with_capacity(bytes.len() + nonascii_count);
                for b in bytes {
                    out.push(b as char);
                }
                Ok(out)
            }
        }
    }
}
