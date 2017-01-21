use super::nif_interface;
use super::nif_interface::{ NIF_ENV, NIF_TERM, NIF_BINARY, c_uint };
use ::wrapper::nif_interface::{ size_t, c_void };

use std::mem::uninitialized;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct ErlNifBinary {
    pub size: size_t,
    pub data: *mut u8,
    pub bin_term: NIF_TERM,
    ref_bin: *mut c_void,
}

impl ErlNifBinary {
    pub unsafe fn new_empty() -> Self {
        ErlNifBinary {
            size: uninitialized(),
            data: uninitialized(),
            bin_term: uninitialized(),
            ref_bin: uninitialized(),
        }
    }
    pub fn as_c_arg(&mut self) -> NIF_BINARY {
        (self as *mut ErlNifBinary) as NIF_BINARY
    }
}

pub unsafe fn alloc(size: size_t) -> Option<ErlNifBinary> {
    let mut binary = unsafe {ErlNifBinary::new_empty()};
    let success = unsafe { nif_interface::enif_alloc_binary(size, binary.as_c_arg()) };
    if success == 0 {
        return None;
    }
    Some(binary)
}

pub unsafe fn realloc(binary: &mut ErlNifBinary, size: size_t) -> bool {
    let success = unsafe { nif_interface::enif_realloc_binary(binary.as_c_arg(), size) };
    success != 0
}
