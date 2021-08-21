use crate::wrapper::{
    NifResourceDtor,  NifResourceFlags, NIF_ENV, NIF_RESOURCE_HANDLE, NIF_RESOURCE_TYPE, NIF_TERM,
};

use rustler_sys::{ErlNifResourceTypeInit, ErlNifResourceDown};
pub use rustler_sys::{
    enif_alloc_resource as alloc_resource, enif_keep_resource as keep_resource,
    enif_make_resource as make_resource, enif_release_resource as release_resource,
};

use std::mem::MaybeUninit;

pub unsafe fn open_resource_type(
    env: NIF_ENV,
    name: &[u8],
    dtor: Option<NifResourceDtor>,
    down: Option<ErlNifResourceDown>,
    flags: NifResourceFlags,
) -> Option<NIF_RESOURCE_TYPE> {
    // Panic if name is not null-terminated.
    assert_eq!(name.last().cloned(), Some(0u8));

    let name_p = name.as_ptr();
    let init = ErlNifResourceTypeInit {
        dtor,
        stop: None,
        down,
        members: 4,
        dyncall: None
    };
    let res = {
        let mut tried = MaybeUninit::uninit();
        rustler_sys::enif_open_resource_type_x(env, name_p, &init, flags, tried.as_mut_ptr())
    };

    if res.is_null() {
        None
    } else {
        Some(res)
    }
}

// Functionally incomplete
pub unsafe fn get_resource(
    env: NIF_ENV,
    term: NIF_TERM,
    typ: NIF_RESOURCE_TYPE,
) -> Option<NIF_RESOURCE_HANDLE> {
    let mut ret_obj = MaybeUninit::uninit();
    let res = rustler_sys::enif_get_resource(env, term, typ, ret_obj.as_mut_ptr());

    if res == 0 {
        None
    } else {
        Some(ret_obj.assume_init())
    }
}
