#![feature(recover)]
#![allow(non_camel_case_types)]

pub mod wrapper;
use wrapper::nif_interface::{NIF_ENV, NIF_TERM, enif_make_badarg, enif_make_atom_len};

#[macro_use]
extern crate lazy_static;

extern crate libc;
pub use libc::{ c_char, size_t, c_int, c_uint, c_void };

use std::marker::PhantomData;

mod types;
pub use self::types::{ NifEncoder, NifDecoder };
pub mod resource;
pub mod binary;
pub mod tuple;
pub mod map;
pub mod atom;
pub mod codegen_runtime;

pub struct NifEnv {
    pub env: NIF_ENV,
}
impl NifEnv {
    pub fn as_c_arg(&self) -> NIF_ENV {
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
                unsafe { enif_make_badarg(env.as_c_arg()) },
            NifError::AllocFail =>
                unsafe { enif_make_badarg(env.as_c_arg()) },
            NifError::Atom(name) => 
                unsafe { enif_make_atom_len(env.as_c_arg(), 
                                            name.as_ptr() as *const u8, 
                                            name.len() as size_t) },
        })
    }
}

#[derive(Clone, Copy)]
pub struct NifTerm<'a> {
    pub term: NIF_TERM,
    env_life: PhantomData<&'a NifEnv>,
}
impl<'a> NifTerm<'a> {
    pub unsafe fn new_raw<'b>(inner: NIF_TERM) -> NifTerm<'b> {
        NifTerm {
            term: inner,
            env_life: PhantomData,
        }
    }
    pub fn new(_env: &'a NifEnv, inner: NIF_TERM) -> Self {
        NifTerm {
            term: inner,
            env_life: PhantomData
        }
    }
    pub fn as_c_arg(&self) -> NIF_TERM {
        self.term
    }
}

