use super::{ NifEnv, NifError, NifResult, NifTerm, NifEncoder, NifDecoder };
use std::mem::uninitialized;
use ::wrapper::nif_interface::{ size_t, c_void };
use ::wrapper::nif_interface::{ NIF_TERM, NIF_BINARY };
use ::wrapper::nif_interface;

#[repr(C)]
#[derive(Clone)]
struct ErlNifBinary {
    pub size: size_t,
    pub data: *mut u8,
    bin_term: NIF_TERM,
    ref_bin: *mut c_void,
}
impl ErlNifBinary {
    unsafe fn new_empty() -> Self {
        ErlNifBinary {
            size: uninitialized(),
            data: uninitialized(),
            bin_term: uninitialized(),
            ref_bin: uninitialized(),
        }
    }
    fn as_c_arg(&mut self) -> NIF_BINARY {
        (self as *mut ErlNifBinary) as NIF_BINARY
    }
}

pub struct OwnedNifBinary {
    inner: ErlNifBinary,
    release: bool,
}
pub struct NifBinary<'a> {
    inner: ErlNifBinary,
    term: NifTerm<'a>,
}

impl Drop for OwnedNifBinary {
    fn drop(&mut self) {
        if self.release {
            unsafe { nif_interface::enif_release_binary(self.inner.as_c_arg()) };
        }
    }
}

impl<'a> OwnedNifBinary {
    pub fn alloc(size: usize) -> Option<OwnedNifBinary> {
        let mut binary = unsafe { ErlNifBinary::new_empty() };
        if unsafe { nif_interface::enif_alloc_binary(
                size as size_t, 
                binary.as_c_arg()) } == 0 {
            return None;
        }
        Some(OwnedNifBinary {
            inner: binary,
            release: true,
        })
    }
    pub fn as_slice(&self) -> &'a [u8] {
        unsafe { ::std::slice::from_raw_parts(self.inner.data, self.inner.size as usize) }
    }
    pub fn as_mut_slice(&mut self) -> &'a mut [u8] {
        unsafe { ::std::slice::from_raw_parts_mut(self.inner.data, self.inner.size as usize) }
    }
    pub fn release<'b>(self, env: &'b NifEnv) -> NifBinary<'b> {
        NifBinary::from_owned(self, env)
    }
}
impl<'a> NifBinary<'a> {
    pub fn from_owned(mut bin: OwnedNifBinary, env: &'a NifEnv) -> Self {
        bin.release = false;
        let term = NifTerm::new(env, unsafe { nif_interface::enif_make_binary(env.as_c_arg(), bin.inner.as_c_arg()) });
        NifBinary {
            inner: bin.inner.clone(),
            term: term,
        }
    }
    pub fn from_term(term: NifTerm<'a>) -> Result<Self, NifError> {
        let mut binary = unsafe { ErlNifBinary::new_empty() };
        if unsafe { nif_interface::enif_inspect_binary(term.get_env().as_c_arg(), term.as_c_arg(), binary.as_c_arg()) } == 0 {
            return Err(NifError::BadArg);
        }
        Ok(NifBinary {
            inner: binary,
            term: term,
        })
    }
    pub fn as_slice(&self) -> &'a [u8] {
        unsafe { ::std::slice::from_raw_parts(self.inner.data, self.inner.size as usize) }
    }
    pub fn get_term<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        self.term.in_env(env)
    }
    pub fn make_subbinary(&self, offset: usize, length: usize) -> NifResult<NifBinary<'a>> {
        let min_len = length.checked_add(offset);
        if try!(min_len.ok_or(NifError::BadArg)) > self.inner.size {
            return Err(NifError::BadArg);
        }

        let raw_term = unsafe { nif_interface::enif_make_sub_binary(self.term.get_env().as_c_arg(), self.inner.bin_term, offset, length) };
        // This should never fail, as we are always passing in a binary term.
        Ok(NifBinary::from_term(NifTerm::new(self.term.get_env(), raw_term)).ok().unwrap())
    }
}

impl<'a> NifDecoder<'a> for NifBinary<'a> {
    fn decode(term: NifTerm<'a>) -> Result<Self, NifError> {
        NifBinary::from_term(term)
    }
}
impl<'a> NifEncoder for NifBinary<'a> {
    fn encode<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        self.get_term(env)
    }
}

/// ## Binary terms
impl<'a> NifTerm<'a> {

    pub fn into_binary(self) -> NifResult<NifBinary<'a>> {
        NifBinary::from_term(self)
    }

}
