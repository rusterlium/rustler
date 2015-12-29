/*
 * This file is meant to directly expose the C nif api. The functions in here
 * should be unsafe, and should not do any validation.
 * Currently we are using ruster_unsafe, but we do want to be able to easily
 * switch to something else in the future if required.
 * 
 * While reexporting from ruster_unsafe directly would remove a lot of code, it
 * is preferred to keep the function signatures in here for future use/reference.
 */

extern crate ruster_unsafe;
extern crate libc;
pub use libc::{ c_int, c_uint, c_double, size_t };

pub type NIF_ENV = *mut ruster_unsafe::ErlNifEnv;
pub type NIF_TERM = ruster_unsafe::ERL_NIF_TERM;
pub type NIF_BINARY = *mut ruster_unsafe::ErlNifBinary;

pub enum NIF_ERROR {
    BAD_ARG
}


pub unsafe fn enif_get_tuple(env: NIF_ENV, term: NIF_TERM, arity: *mut c_int, array: *mut *const NIF_TERM) -> c_int {
    ruster_unsafe::enif_get_tuple(env, term, arity, array)
}

pub unsafe fn enif_make_badarg(env: NIF_ENV) -> NIF_TERM {
    ruster_unsafe::enif_make_badarg(env)
}

pub unsafe fn enif_make_atom_len(env: NIF_ENV, string: *const u8, length: size_t) -> NIF_TERM {
    ruster_unsafe::enif_make_atom_len(env, string, length)
}

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
