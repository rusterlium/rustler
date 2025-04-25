#![allow(clippy::missing_safety_doc)]
#![allow(clippy::upper_case_acronyms)]

pub use std::ffi::{c_char, c_int, c_uint, c_void};

#[allow(non_camel_case_types)]
pub type size_t = usize;

#[allow(non_camel_case_types)]
pub type ERL_NIF_UINT = size_t;

#[allow(non_camel_case_types)]
pub type ERL_NIF_TERM = ERL_NIF_UINT;

/// See [ErlNifEnv](http://www.erlang.org/doc/man/erl_nif.html#ErlNifEnv) in the Erlang docs.
#[derive(Debug)]
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifEnv {
    dummy: *mut c_void, // block automatic Send and Sync traits.  Ref https://doc.rust-lang.org/beta/nomicon/send-and-sync.html
}

// Ownership of an env may be safely transfers between threads, therefore ErlNifEnv is Send.
// This is the common use case for process independent environments created with enif_alloc_env().
// ErlNifEnv is NOT Sync because it is thread unsafe.
unsafe impl Send for ErlNifEnv {}

/// See [ErlNifFunc](http://www.erlang.org/doc/man/erl_nif.html#ErlNifFunc) in the Erlang docs.
// #[allow(missing_copy_implementations)]
#[derive(Debug)]
#[repr(C)]
pub struct ErlNifFunc {
    pub name: *const c_char,
    pub arity: c_uint,
    pub function: unsafe extern "C" fn(
        env: *mut ErlNifEnv,
        argc: c_int,
        argv: *const ERL_NIF_TERM,
    ) -> ERL_NIF_TERM,
    pub flags: c_uint,
}

// #[allow(missing_copy_implementations)]
#[doc(hidden)]
#[derive(Debug)]
#[repr(C)]
#[allow(non_snake_case)]
pub struct ErlNifEntry {
    pub major: c_int,
    pub minor: c_int,
    pub name: *const c_char,
    pub num_of_funcs: c_int,
    pub funcs: *const ErlNifFunc,
    pub load: Option<
        unsafe extern "C" fn(
            env: *mut ErlNifEnv,
            priv_data: *mut *mut c_void,
            load_info: ERL_NIF_TERM,
        ) -> c_int,
    >,
    pub reload: Option<
        unsafe extern "C" fn(
            env: *mut ErlNifEnv,
            priv_data: *mut *mut c_void,
            load_info: ERL_NIF_TERM,
        ) -> c_int,
    >,
    pub upgrade: Option<
        unsafe extern "C" fn(
            env: *mut ErlNifEnv,
            priv_data: *mut *mut c_void,
            old_priv_data: *mut *mut c_void,
            load_info: ERL_NIF_TERM,
        ) -> c_int,
    >,
    pub unload: Option<unsafe extern "C" fn(env: *mut ErlNifEnv, priv_data: *mut c_void) -> ()>,
    pub vm_variant: *const c_char,
    pub options: c_uint,                      // added in 2.7
    pub sizeof_ErlNifResourceTypeInit: usize, // added in 2.12
}
