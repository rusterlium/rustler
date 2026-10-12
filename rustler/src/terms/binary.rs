//! Byte-aligned binary terms, without an inspected byte slice.

use crate::sys::enif_inspect_binary;
use crate::{Encoder, Env, Error, NifResult};
use std::mem::MaybeUninit;

bound_term!(Binary, crate::Term::is_binary);

impl<'a> Binary<'a> {
    /// Inspect the binary to access its byte slice through the high-level binary API.
    pub fn into_binary<'b>(self, env: Env<'b>) -> NifResult<crate::Binary<'b>> {
        let term = self.encode(env);
        let mut binary = MaybeUninit::uninit();
        if unsafe { enif_inspect_binary(env.as_c_arg(), term.as_c_arg(), binary.as_mut_ptr()) } == 0
        {
            return Err(Error::BadArg);
        }
        let binary = unsafe { binary.assume_init() };
        unsafe {
            let bytes = std::slice::from_raw_parts(binary.data, binary.size);
            Ok(crate::Binary::from_term_and_slice(term, bytes))
        }
    }
}
