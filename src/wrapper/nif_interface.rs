#![allow(dead_code)]

//! This module is meant to directly expose the C nif api. The functions in here
//! should be unsafe, and should not do any validation.
//! Currently we are using ruster_unsafe, but we do want to be able to easily
//! switch to something else in the future if required.
//! 
//! While reexporting from ruster_unsafe directly would remove a lot of code, it
//! is preferred to keep the function signatures in here for future use/reference.

extern crate ruster_unsafe;
extern crate libc;
pub use libc::{ c_int, c_uint, c_double, size_t, c_void, c_uchar };

pub type NIF_ENV = *mut ruster_unsafe::ErlNifEnv;
pub type NIF_TERM = *const c_void;
pub type NIF_BINARY = *mut ruster_unsafe::ErlNifBinary;
pub type NIF_RESOURCE_TYPE = *mut ruster_unsafe::ErlNifResourceType;
pub type NIF_RESOURCE_HANDLE = *mut c_void;

pub type NifResourceDtor = extern "C" fn(r_env: NIF_ENV, obj: NIF_RESOURCE_HANDLE) -> ();
pub type NifResourceFlags = ruster_unsafe::ErlNifResourceFlags;

pub enum NIF_ERROR {
    BAD_ARG
}

pub type DEF_NIF_FUNC = ruster_unsafe::ErlNifFunc;
pub type DEF_NIF_ENTRY = ruster_unsafe::ErlNifEntry;
pub use self::ruster_unsafe::NIF_MAJOR_VERSION;
pub use self::ruster_unsafe::NIF_MINOR_VERSION;
pub use self::ruster_unsafe::ErlNifResourceFlags as NIF_RESOURCE_FLAGS;


pub unsafe fn enif_make_badarg(env: NIF_ENV) -> NIF_TERM {
    ruster_unsafe::enif_make_badarg(env)
}

pub unsafe fn enif_alloc_env() -> NIF_ENV {
    ruster_unsafe::enif_alloc_env()
}

pub unsafe fn enif_raise_exception(env: NIF_ENV, reason: NIF_TERM) -> NIF_TERM {
    ruster_unsafe::enif_raise_exception(env, reason)
}

pub unsafe fn enif_make_copy(dest_env: NIF_ENV, source_term: NIF_TERM) -> NIF_TERM {
    ruster_unsafe::enif_make_copy(dest_env, source_term)
}

// Term type checks
pub unsafe fn enif_is_atom(env: NIF_ENV, term: NIF_TERM) -> c_int {
    ruster_unsafe::enif_is_atom(env, term)
}
pub unsafe fn enif_is_list(env: NIF_ENV, term: NIF_TERM) -> c_int {
    ruster_unsafe::enif_is_list(env, term)
}
pub unsafe fn enif_is_empty_list(env: NIF_ENV, term: NIF_TERM) -> c_int {
    ruster_unsafe::enif_is_empty_list(env, term)
}

// Atoms
pub unsafe fn enif_make_atom_len(env: NIF_ENV, string: *const u8, length: size_t) -> NIF_TERM {
    ruster_unsafe::enif_make_atom_len(env, string, length)
}

// Binaries
pub unsafe fn enif_release_binary(bin_ref: NIF_BINARY) {
    ruster_unsafe::enif_release_binary(bin_ref)
}
pub unsafe fn enif_alloc_binary(size: size_t, bin_ref: NIF_BINARY) -> c_int {
    ruster_unsafe::enif_alloc_binary(size, bin_ref)
}
pub unsafe fn enif_make_binary(env: NIF_ENV, bin_ref: NIF_BINARY) -> NIF_TERM {
    ruster_unsafe::enif_make_binary(env, bin_ref)
}
pub unsafe fn enif_inspect_binary(env: NIF_ENV, term: NIF_TERM, bin_ref: NIF_BINARY) -> c_int {
    ruster_unsafe::enif_inspect_binary(env, term, bin_ref)
}

// Maps
pub unsafe fn enif_get_map_value(env: NIF_ENV, map: NIF_TERM, key: NIF_TERM, value: *mut NIF_TERM) -> c_int {
    ruster_unsafe::enif_get_map_value(env, map, key, value)
}
pub unsafe fn enif_get_map_size(env: NIF_ENV, map: NIF_TERM, size: *mut size_t) -> c_int {
    ruster_unsafe::enif_get_map_size(env, map, size)
}
pub unsafe fn enif_make_new_map(env: NIF_ENV) -> NIF_TERM {
    ruster_unsafe::enif_make_new_map(env)
}
pub unsafe fn enif_make_map_put(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, value: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    ruster_unsafe::enif_make_map_put(env, map_in, key, value, map_out)
}
pub unsafe fn enif_make_map_update(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, value: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    ruster_unsafe::enif_make_map_update(env, map_in, key, value, map_out)
}
pub unsafe fn enif_make_map_remove(env: NIF_ENV, map_in: NIF_TERM, key: NIF_TERM, map_out: *mut NIF_TERM) -> c_int {
    ruster_unsafe::enif_make_map_remove(env, map_in, key, map_out)
}

// Tuple
pub unsafe fn enif_get_tuple(env: NIF_ENV, term: NIF_TERM, arity: *mut c_int, array: *mut *const NIF_TERM) -> c_int {
    ruster_unsafe::enif_get_tuple(env, term, arity, array)
}
pub unsafe fn enif_make_tuple_from_array(env: NIF_ENV, terms: *const NIF_TERM, terms_len: c_uint) -> NIF_TERM {
    ruster_unsafe::enif_make_tuple_from_array(env, terms, terms_len)
}

// List
pub unsafe fn enif_get_list_cell(env: NIF_ENV, list: NIF_TERM, head: *mut NIF_TERM, tail: *mut NIF_TERM) -> c_int {
    ruster_unsafe::enif_get_list_cell(env, list, head, tail)
}
pub unsafe fn enif_get_list_length(env: NIF_ENV, list: NIF_TERM, length: *mut c_uint) -> c_int {
    ruster_unsafe::enif_get_list_length(env, list, length)
}

// Resources
pub unsafe fn enif_open_resource_type(env: NIF_ENV, module_str: *const c_uchar, name: *const c_uchar,
                                      dtor: Option<NifResourceDtor>, flags: NifResourceFlags, tried: *mut NifResourceFlags
                                      ) -> NIF_RESOURCE_TYPE {
    ruster_unsafe::enif_open_resource_type(env, module_str, name, dtor, flags, tried)
}
pub unsafe fn enif_alloc_resource(typ: NIF_RESOURCE_TYPE, size: usize) -> NIF_RESOURCE_HANDLE {
    ruster_unsafe::enif_alloc_resource(typ, size)
}
pub unsafe fn enif_make_resource(env: NIF_ENV, obj: NIF_RESOURCE_HANDLE) -> NIF_TERM {
    ruster_unsafe::enif_make_resource(env, obj)
}
pub unsafe fn enif_get_resource(env: NIF_ENV, term: NIF_TERM, typ: NIF_RESOURCE_TYPE, objp: *mut NIF_RESOURCE_HANDLE) -> c_int {
    ruster_unsafe::enif_get_resource(env, term, typ, objp)
}
pub unsafe fn enif_release_resource(obj: NIF_RESOURCE_HANDLE) {
    ruster_unsafe::enif_release_resource(obj)
}
pub unsafe fn enif_keep_resource(obj: NIF_RESOURCE_HANDLE) {
    ruster_unsafe::enif_keep_resource(obj)
}

// Numbers
macro_rules! wrap_number {
    ($typ: ty, $encode: ident, $decode: ident) => {
        pub unsafe fn $encode(env: NIF_ENV, val: $typ) -> NIF_TERM {
            ruster_unsafe::$encode(env, val)
        }
        pub unsafe fn $decode(env: NIF_ENV, term: NIF_TERM, dest: *mut $typ) -> c_int {
            ruster_unsafe::$decode(env, term, dest)
        }
    }
}
wrap_number!(c_int, enif_make_int, enif_get_int);
wrap_number!(c_uint, enif_make_uint, enif_get_uint);
wrap_number!(i64, enif_make_int64, enif_get_int64);
wrap_number!(u64, enif_make_uint64, enif_get_uint64);
wrap_number!(c_double, enif_make_double, enif_get_double);
