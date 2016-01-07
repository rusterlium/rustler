#![feature(recover, std_panic)]
#![allow(non_camel_case_types)]

mod wrapper;

#[macro_use]
extern crate lazy_static;

extern crate ruster_unsafe;
pub use self::ruster_unsafe::{ ERL_NIF_TERM, ErlNifResourceFlags, ErlNifResourceType };

pub mod ruster_export {
    pub use super::ruster_unsafe::*;
}

extern crate libc;
pub use libc::{ c_char, size_t, c_int, c_uint, c_void };

use std::marker::PhantomData;

mod types;
pub use self::types::{ NifEncoder, NifDecoder };

mod resource;
pub use self::resource::{ open_resource_type_raw, alloc_resource_raw };
pub use self::resource::{ open_struct_resource_type, alloc_struct_resource, get_struct_resource };

mod binary;
pub use self::binary::{ NifBinary, alloc_binary, make_binary, get_binary };

//#[macro_reexport]
pub mod tuple;

pub mod map;

pub mod atom;
pub use self::atom::{ init_atom, get_atom, get_atom_init };

pub mod codegen_runtime;

pub struct NifEnv {
    pub env: *mut ruster_unsafe::ErlNifEnv,
}
impl NifEnv {
    pub fn as_c_arg(&self) -> *mut ruster_unsafe::ErlNifEnv {
        self.env
    }
}

#[derive(Clone, Copy)]
pub enum NifError {
    BadArg,
    AllocFail,
    Atom(&'static str),
}
impl NifError {
    pub fn to_term<'a>(self, env: &'a NifEnv) -> NifTerm<'a> {
        NifTerm::new(env, match self {
            NifError::BadArg => 
                unsafe { ruster_export::enif_make_badarg(env.as_c_arg()) },
            NifError::AllocFail =>
                unsafe { ruster_export::enif_make_badarg(env.as_c_arg()) },
            NifError::Atom(name) => 
                unsafe { ruster_export::enif_make_atom_len(env.as_c_arg(), 
                                                           name.as_ptr() as *const u8, 
                                                           name.len() as size_t) },
        })
    }
}

#[derive(Clone, Copy)]
pub struct NifTerm<'a> {
    pub term: ERL_NIF_TERM,
    env_life: PhantomData<&'a NifEnv>,
}
impl<'a> NifTerm<'a> {
    pub fn new(_env: &'a NifEnv, inner: ERL_NIF_TERM) -> Self {
        NifTerm {
            term: inner,
            env_life: PhantomData
        }
    }
    pub fn as_c_arg(&self) -> ERL_NIF_TERM {
        self.term
    }
}

pub struct NifResourceType {
    pub res: *mut ErlNifResourceType
}
pub struct NifStructResourceType<T> {
    pub res: NifResourceType,
    pub struct_type: PhantomData<T>,
}



/*pub fn decode_type<T: NifDecoder>(term: NifTerm, env: &NifEnv) -> Result<T, NifError> {
    NifDecoder::decode(term, env)
}*/

/*#[macro_export]
macro_rules! nif_atom {
    ($env:expr, $name:ident) => ({
        const atom_name: &'static str = stringify!($name);
        $crate::ruster_export::enif_make_atom_len(
            $env.env,
            atom_name.as_ptr() as *const u8,
            atom_name.len() as $crate::size_t)
    });
}*/

/*pub fn nif_atom<'a>(env: &'a NifEnv, name: &str) -> NifTerm<'a> {
    unsafe { 
        NifTerm::new(env, ruster_unsafe::enif_make_atom_len(
            env.env,
            name.as_ptr() as *const u8,
            name.len() as size_t))
    }
}*/
