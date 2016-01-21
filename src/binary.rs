extern crate ruster_unsafe;

use super::{ NifEnv, NifError, NifTerm, NifEncoder, NifDecoder };
use libc::{ size_t, c_void };
use std::mem::uninitialized;
use ::wrapper::nif_interface::NIF_TERM;

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
    fn as_c_arg(&mut self) -> *mut ruster_unsafe::ErlNifBinary {
        (self as *mut ErlNifBinary) as *mut ruster_unsafe::ErlNifBinary
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
            unsafe { ruster_unsafe::enif_release_binary(self.inner.as_c_arg()) };
        }
    }
}

impl<'a> OwnedNifBinary {
    pub fn alloc(size: usize) -> Option<OwnedNifBinary> {
        let mut binary = unsafe { ErlNifBinary::new_empty() };
        if unsafe { ruster_unsafe::enif_alloc_binary(
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
        let term = NifTerm::new(env, unsafe { ruster_unsafe::enif_make_binary(env.as_c_arg(), bin.inner.as_c_arg()) });
        NifBinary {
            inner: bin.inner.clone(),
            term: term,
        }
    }
    pub fn from_term(term: NifTerm<'a>, env: &NifEnv) -> Result<Self, NifError> {
        let mut binary = unsafe { ErlNifBinary::new_empty() };
        if unsafe { ruster_unsafe::enif_inspect_binary(env.as_c_arg(), term.as_c_arg(), binary.as_c_arg()) } == 0 {
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
    pub fn get_term(&self, _env: &'a NifEnv) -> NifTerm<'a> {
        self.term
    }
}

impl<'a> NifDecoder<'a> for NifBinary<'a> {
    fn decode(term: NifTerm<'a>, env: &NifEnv) -> Result<Self, NifError> {
        NifBinary::from_term(term, env)
    }
}
