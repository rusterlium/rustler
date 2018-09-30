use super::nif_interface;
use super::nif_interface::NIF_BINARY;
use wrapper::nif_interface::{c_void, size_t};

use std::mem::uninitialized;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct ErlNifBinary {
    pub size: size_t,
    pub data: *mut u8,
    _internal: [*mut c_void; 3],
}

impl ErlNifBinary {
    pub unsafe fn new_empty() -> Self {
        ErlNifBinary {
            size: uninitialized(),
            data: uninitialized(),
            _internal: uninitialized(),
        }
    }
    pub fn as_c_arg(&mut self) -> NIF_BINARY {
        (self as *mut ErlNifBinary) as NIF_BINARY
    }
}

pub unsafe fn alloc(size: size_t) -> Option<ErlNifBinary> {
    let mut binary = ErlNifBinary::new_empty();
    let success = nif_interface::enif_alloc_binary(size, binary.as_c_arg());
    if success == 0 {
        return None;
    }
    Some(binary)
}

pub unsafe fn realloc(binary: &mut ErlNifBinary, size: size_t) -> bool {
    let success = nif_interface::enif_realloc_binary(binary.as_c_arg(), size);
    success != 0
}
