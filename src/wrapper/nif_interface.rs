#![allow(dead_code)]

//! This module is meant to directly expose the C nif api. The functions in here
//! should be unsafe, and should not do any validation.
//! Currently we are using erlang_nif_sys, but we do want to be able to easily
//! switch to something else in the future if required.
//!
//! While reexporting from erlang_nif_sys directly would remove a lot of code, it
//! is preferred to keep the function signatures in here for future use/reference.

extern crate erlang_nif_sys;
extern crate std;
//extern crate libc;
//pub use libc::{ c_int, c_uint, c_double, size_t, c_void, c_uchar };
pub use std::os::raw::{ c_int, c_uint, c_double, c_void, c_uchar };
pub type size_t = usize;

pub type NIF_ENV = *mut erlang_nif_sys::ErlNifEnv;
pub type NIF_TERM = size_t;
pub type NIF_BINARY = *mut erlang_nif_sys::ErlNifBinary;
pub type NIF_RESOURCE_TYPE = *const erlang_nif_sys::ErlNifResourceType;
pub type NIF_PID = *const erlang_nif_sys::ErlNifPid;

pub type NIF_RESOURCE_HANDLE = *const c_void;
pub type MUTABLE_NIF_RESOURCE_HANDLE = *mut c_void;

pub type NifResourceDtor = extern "C" fn(r_env: NIF_ENV, obj: MUTABLE_NIF_RESOURCE_HANDLE) -> ();
pub type NifResourceFlags = erlang_nif_sys::ErlNifResourceFlags;

pub enum NIF_ERROR {
    BAD_ARG
}

pub type DEF_NIF_FUNC = erlang_nif_sys::ErlNifFunc;
pub type DEF_NIF_ENTRY = erlang_nif_sys::ErlNifEntry;
pub use self::erlang_nif_sys::NIF_MAJOR_VERSION;
pub use self::erlang_nif_sys::NIF_MINOR_VERSION;
pub use self::erlang_nif_sys::ErlNifResourceFlags as NIF_RESOURCE_FLAGS;

#[repr(C)]
pub enum ErlNifTaskFlags {
    ERL_NIF_NORMAL_JOB = 0,
    ERL_NIF_DIRTY_JOB_CPU_BOUND = 1,
    ERL_NIF_DIRTY_JOB_IO_BOUND = 2
}

pub unsafe fn enif_make_badarg(env: NIF_ENV) -> NIF_TERM {
    erlang_nif_sys::enif_make_badarg(env)
}

pub unsafe fn enif_alloc_env() -> NIF_ENV {
    erlang_nif_sys::enif_alloc_env()
}

pub unsafe fn enif_raise_exception(env: NIF_ENV, reason: NIF_TERM) -> NIF_TERM {
    erlang_nif_sys::enif_raise_exception(env, reason)
}

pub unsafe fn enif_make_copy(dest_env: NIF_ENV, source_term: NIF_TERM) -> NIF_TERM {
    erlang_nif_sys::enif_make_copy(dest_env, source_term)
}

// Term type checks
pub unsafe fn enif_is_atom(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_atom(env, term)
}
pub unsafe fn enif_is_binary(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_binary(env, term)
}
pub unsafe fn enif_is_empty_list(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_empty_list(env, term)
}
pub unsafe fn enif_is_exception(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_exception(env, term)
}
pub unsafe fn enif_is_fun(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_fun(env, term)
}
pub unsafe fn enif_is_list(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_list(env, term)
}
pub unsafe fn enif_is_map(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_map(env, term)
}
pub unsafe fn enif_is_pid(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_pid(env, term)
}
pub unsafe fn enif_is_port(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_port(env, term)
}
pub unsafe fn enif_is_ref(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_ref(env, term)
}
pub unsafe fn enif_is_tuple(env: NIF_ENV, term: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_is_tuple(env, term)
}

// Atoms
pub unsafe fn enif_make_atom_len(env: NIF_ENV, string: *const u8, length: size_t) -> NIF_TERM {
    erlang_nif_sys::enif_make_atom_len(env, string, length)
}
pub unsafe fn enif_get_atom_latin1(env: NIF_ENV, term: NIF_TERM, buf: *mut u8, size: c_uint) -> c_int {
    erlang_nif_sys::enif_get_atom(env, term, buf, size, erlang_nif_sys::ErlNifCharEncoding::ERL_NIF_LATIN1)
}
pub unsafe fn enif_get_atom_length_latin1(env: NIF_ENV, term: NIF_TERM, len: *mut c_uint) -> c_int {
    erlang_nif_sys::enif_get_atom_length(env, term, len, erlang_nif_sys::ErlNifCharEncoding::ERL_NIF_LATIN1)
}

// Binaries
pub unsafe fn enif_release_binary(bin_ref: NIF_BINARY) {
    erlang_nif_sys::enif_release_binary(bin_ref)
}
pub unsafe fn enif_alloc_binary(size: size_t, bin_ref: NIF_BINARY) -> c_int {
    erlang_nif_sys::enif_alloc_binary(size, bin_ref)
}
pub unsafe fn enif_make_binary(env: NIF_ENV, bin_ref: NIF_BINARY) -> NIF_TERM {
    erlang_nif_sys::enif_make_binary(env, bin_ref)
}
pub unsafe fn enif_inspect_binary(env: NIF_ENV, term: NIF_TERM, bin_ref: NIF_BINARY) -> c_int {
    erlang_nif_sys::enif_inspect_binary(env, term, bin_ref)
}
pub unsafe fn enif_make_sub_binary(env: NIF_ENV, bin_term: NIF_TERM, pos: size_t, size: size_t) -> NIF_TERM {
    erlang_nif_sys::enif_make_sub_binary(env, bin_term, pos, size)
}

// Maps
pub unsafe fn enif_get_map_value(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM, value: *mut NIF_TERM) -> c_int {
    erlang_nif_sys::enif_get_map_value(env, map, key, value)
}
pub unsafe fn enif_get_map_size(env: NIF_ENV, map: NIF_TERM, size: *mut size_t) -> c_int {
    erlang_nif_sys::enif_get_map_size(env, map, size)
}
pub unsafe fn enif_make_new_map(env: NIF_ENV) -> NIF_TERM {
    erlang_nif_sys::enif_make_new_map(env)
}
pub unsafe fn enif_make_map_put(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, value: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    erlang_nif_sys::enif_make_map_put(env, map_in, key, value, map_out)
}
pub unsafe fn enif_make_map_update(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, value: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    erlang_nif_sys::enif_make_map_update(env, map_in, key, value, map_out)
}
pub unsafe fn enif_make_map_remove(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    erlang_nif_sys::enif_make_map_remove(env, map_in, key, map_out)
}

// Tuple
pub unsafe fn enif_get_tuple(env: NIF_ENV, term: NIF_TERM, arity: *mut c_int, array: *mut *const NIF_TERM) -> c_int {
    erlang_nif_sys::enif_get_tuple(env, term, arity, array)
}
pub unsafe fn enif_make_tuple_from_array(env: NIF_ENV, terms: *const NIF_TERM, terms_len: c_uint) -> NIF_TERM {
    erlang_nif_sys::enif_make_tuple_from_array(env, terms, terms_len)
}

// List
pub unsafe fn enif_get_list_cell(env: NIF_ENV, list: NIF_TERM, head: *mut NIF_TERM, tail: *mut NIF_TERM) -> c_int {
    erlang_nif_sys::enif_get_list_cell(env, list, head, tail)
}
pub unsafe fn enif_get_list_length(env: NIF_ENV, list: NIF_TERM, length: *mut c_uint) -> c_int {
    erlang_nif_sys::enif_get_list_length(env, list, length)
}
pub unsafe fn enif_make_list_from_array(env: NIF_ENV, arr: *const NIF_TERM, cnt: c_uint) -> NIF_TERM {
    erlang_nif_sys::enif_make_list_from_array(env, arr, cnt)
}
pub unsafe fn enif_make_list_cell(env: NIF_ENV, head: NIF_TERM, tail: NIF_TERM) -> NIF_TERM {
    erlang_nif_sys::enif_make_list_cell(env, head, tail)
}
pub unsafe fn enif_make_reverse_list(env: NIF_ENV, list_in: NIF_TERM, list_out: *mut NIF_TERM) -> c_int {
    erlang_nif_sys::enif_make_reverse_list(env, list_in, list_out)
}

// Resources
pub unsafe fn enif_open_resource_type(env: NIF_ENV, module_str: *const c_uchar, name: *const c_uchar,
                                      dtor: Option<NifResourceDtor>, flags: NifResourceFlags, tried: *mut NifResourceFlags
                                      ) -> NIF_RESOURCE_TYPE {
    erlang_nif_sys::enif_open_resource_type(env, module_str, name, dtor, flags, tried)
}
pub unsafe fn enif_alloc_resource(typ: NIF_RESOURCE_TYPE, size: usize) -> NIF_RESOURCE_HANDLE {
    erlang_nif_sys::enif_alloc_resource(typ, size)
}
pub unsafe fn enif_make_resource(env: NIF_ENV, obj: NIF_RESOURCE_HANDLE) -> NIF_TERM {
    erlang_nif_sys::enif_make_resource(env, obj)
}
pub unsafe fn enif_get_resource(env: NIF_ENV, term: NIF_TERM, typ: NIF_RESOURCE_TYPE, objp: *mut NIF_RESOURCE_HANDLE) -> c_int {
    erlang_nif_sys::enif_get_resource(env, term, typ, objp)
}
pub unsafe fn enif_release_resource(obj: NIF_RESOURCE_HANDLE) {
    erlang_nif_sys::enif_release_resource(obj)
}
pub unsafe fn enif_keep_resource(obj: NIF_RESOURCE_HANDLE) {
    erlang_nif_sys::enif_keep_resource(obj)
}

pub use self::erlang_nif_sys::{
    ErlNifMapIterator,
    ErlNifMapIteratorEntry,
    ErlNifPid,
    enif_clear_env,
    enif_free_env,
    enif_get_local_pid,
    enif_make_pid,
    enif_map_iterator_create,
    enif_map_iterator_destroy,
    enif_map_iterator_get_pair,
    enif_map_iterator_next,
    enif_self,
};

// Scheduling
pub unsafe fn enif_consume_timeslice(env: NIF_ENV, percent: c_int) -> c_int {
    erlang_nif_sys::enif_consume_timeslice(env, percent)
}

pub unsafe fn enif_schedule_nif(env: NIF_ENV,
                                fun_name: *const c_uchar,
                                flags: c_int,
                                dtor: Option<extern fn(env: NIF_ENV, argc: c_int, argv: *const NIF_TERM)>,
                                argc: c_int,
                                argv: *const NIF_TERM) -> NIF_TERM {
    erlang_nif_sys::enif_schedule_nif(env, fun_name, flags, dtor, argc, argv)
}

// Processes
pub unsafe fn enif_send(env: NIF_ENV,
                        to_pid: NIF_PID,
                        msg_env: NIF_ENV,
                        msg: NIF_TERM) -> c_int {
    erlang_nif_sys::enif_send(env, to_pid, msg_env, msg)
}

// Numbers
macro_rules! wrap_number {
    ($typ: ty, $encode: ident, $decode: ident) => {
        pub unsafe fn $encode(env: NIF_ENV, val: $typ) -> NIF_TERM {
            erlang_nif_sys::$encode(env, val)
        }
        pub unsafe fn $decode(env: NIF_ENV, term: NIF_TERM, dest: *mut $typ) -> c_int {
            erlang_nif_sys::$decode(env, term, dest)
        }
    }
}
wrap_number!(c_int, enif_make_int, enif_get_int);
wrap_number!(c_uint, enif_make_uint, enif_get_uint);
wrap_number!(i64, enif_make_int64, enif_get_int64);
wrap_number!(u64, enif_make_uint64, enif_get_uint64);
wrap_number!(c_double, enif_make_double, enif_get_double);
