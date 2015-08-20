extern crate ruster_unsafe;
use ruster_unsafe::ERL_NIF_TERM;

use super::{ NifEnv, NifError, NifTerm };
use libc::{ size_t, c_void };
use std::mem::uninitialized;
use std::marker::PhantomData;

#[repr(C)]
struct ErlNifBinary {
    pub size: size_t,
    pub data: *mut u8,
    bin_term: ERL_NIF_TERM,
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
    unsafe fn as_c_arg(&mut self) -> *mut ruster_unsafe::ErlNifBinary {
        (self as *mut ErlNifBinary) as *mut ruster_unsafe::ErlNifBinary
    }
}

pub struct NifBinary<'a> {
    desc: ErlNifBinary,
    owned: bool,
    lifetime: PhantomData<&'a NifEnv>,
}
impl<'a> Drop for NifBinary<'a> {
    fn drop(&mut self) {
        if self.owned {
            unsafe { ruster_unsafe::enif_release_binary(self.desc.as_c_arg()) };
        }
    }
}
impl<'a> NifBinary<'a> {
    unsafe fn as_c_arg(&mut self) -> *mut ruster_unsafe::ErlNifBinary {
        self.desc.as_c_arg()
    }
    fn as_slice(&self) -> &[u8] {
        unsafe { ::std::slice::from_raw_parts(self.desc.data, self.desc.size as usize) }
    }
    fn as_mut_slice(&mut self) -> &mut [u8] {
        if !self.owned {
            panic!("Tried to access a nonowned NIF Binary as a mutable slice");
        }
        unsafe { ::std::slice::from_raw_parts_mut(self.desc.data, self.desc.size as usize) }
    }
}

pub fn alloc_binary<'a>(_env: &'a NifEnv, size: usize) -> Result<NifBinary<'a>, NifError> {
    let mut binary = unsafe { ErlNifBinary::new_empty() };
    if unsafe { ruster_unsafe::enif_alloc_binary(
                    size as size_t, 
                    binary.as_c_arg()) } == 0 {
        return Err(NifError::AllocFail);
    }
    Ok(NifBinary {
        desc: binary,
        owned: true,
        lifetime: PhantomData,
    })
}

pub fn make_binary<'a>(env: &'a NifEnv, binary: &mut NifBinary) -> NifTerm<'a> {
    binary.owned = false;
    NifTerm::new(env, 
                 unsafe { ruster_unsafe::enif_make_binary(env.as_c_arg(), binary.as_c_arg()) })
}

pub fn get_binary<'a>(env: &'a NifEnv, term: NifTerm) -> Result<NifBinary<'a>, NifError> {
    let mut binary = unsafe { ErlNifBinary::new_empty() };
    if unsafe { ruster_unsafe::enif_inspect_binary(env.as_c_arg(), term.as_c_arg(), binary.as_c_arg()) } == 0 {
        return Err(NifError::BadArg);
    }
    Ok(NifBinary {
        desc: binary,
        owned: false,
        lifetime: PhantomData,
    })
}
