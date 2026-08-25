pub(crate) use crate::sys::ErlNifBinary;
use crate::sys::{enif_alloc_binary, enif_make_new_binary, enif_realloc_binary};
use crate::{Env, Term};
use std::alloc::handle_alloc_error;
use std::mem::MaybeUninit;

use crate::alloc::array_layout;

pub use crate::sys::enif_make_sub_binary as make_subbinary;

pub unsafe fn alloc(size: usize) -> ErlNifBinary {
    let mut binary = MaybeUninit::uninit();
    let success = enif_alloc_binary(size, binary.as_mut_ptr());
    if success == 0 {
        handle_alloc_error(array_layout::<u8>(size));
    }
    binary.assume_init()
}

pub unsafe fn realloc(binary: &mut ErlNifBinary, size: usize) -> bool {
    let success = enif_realloc_binary(binary, size);
    success != 0
}

pub unsafe fn new_binary(env: Env, size: usize) -> (*mut u8, Term) {
    let mut term = MaybeUninit::uninit();
    let buf = enif_make_new_binary(env.as_c_arg(), size, term.as_mut_ptr());
    if buf.is_null() {
        handle_alloc_error(array_layout::<u8>(size));
    }
    (buf, Term::new(env, term.assume_init()))
}
