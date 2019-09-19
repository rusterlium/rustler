use crate::wrapper::size_t;
pub(in crate) use rustler_sys::ErlNifBinary;
use std::mem::MaybeUninit;

pub unsafe fn alloc(size: size_t) -> Option<ErlNifBinary> {
    let mut binary = MaybeUninit::uninit();
    let success = rustler_sys::enif_alloc_binary(size, binary.as_mut_ptr());
    if success == 0 {
        return None;
    }
    Some(binary.assume_init())
}

pub unsafe fn realloc(binary: &mut ErlNifBinary, size: size_t) -> bool {
    let success = rustler_sys::enif_realloc_binary(binary, size);
    success != 0
}
