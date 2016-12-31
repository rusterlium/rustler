#![allow(non_camel_case_types)]

//! [Github](https://github.com/hansihe/Rustler)
//! [Example](https://github.com/hansihe/Rustler_Example)
//!
//! Rustler is a library for writing Erlang NIFs in safe Rust code. That means there should be no
//! ways to crash the BEAM (Erlang VM). The library provides facilities for generating the
//! boilerplate for interacting with the BEAM, handles encoding and decoding of Erlang terms, and
//! catches rust panics before they unwind into C.
//!
//! The library provides functionality for both Erlang and Elixir, however Elixir is favored as of
//! now.
//!
//! This crate provides the entire runtime library for rustler. Code generators are located in the
//! rustler_codegen library.
//!
//! # Getting Started
//! There is a [`:rustler`](https://hex.pm/packages/rustler) package on hex.pm that provides
//! functionality which makes working with Rustler easier, including project generators, an
//! automatic NIF compiler for Mix, and utilities for loading the compiled NIF.
//!
//! For more information about this, see [the documentation for
//! rustler_mix](https://hexdocs.pm/rustler/basics.html).

pub mod wrapper;
use wrapper::nif_interface::{NIF_ENV, NIF_TERM, enif_make_badarg, enif_make_atom_len};
pub use wrapper::nif_interface::size_t;
pub use wrapper::nif_interface::ErlNifTaskFlags;

#[macro_use]
extern crate lazy_static;

mod types;
pub use self::types::{ NifEncoder, NifDecoder };
pub mod resource;
pub mod binary;
pub mod tuple;
pub mod map;
pub mod list;
pub mod atom;
pub mod codegen_runtime;
pub mod ex_struct;
pub mod dynamic;
pub mod schedule;

mod export;

pub type NifResult<T> = Result<T, NifError>;

/// On each NIF call, a NifEnv is passed in. The NifEnv is used for most operations that involve
/// communicating with the BEAM, like decoding and encoding terms.
///
/// There is no way to allocate a NifEnv at the moment, but this may be possible in the future.
#[derive(PartialEq)]
pub struct NifEnv {
    env: NIF_ENV,
}
impl NifEnv {
    pub fn as_c_arg(&self) -> NIF_ENV {
        self.env
    }
}

/// Represents usual errors that can happen in a nif. This enables you to return an error from
/// anywhere, even places where you don't have an NifEnv availible.
#[derive(Clone, Copy)]
pub enum NifError {
    /// Returned when the NIF has been called with the wrong number or type of arguments.
    BadArg,
    /// Returned when an allocation fails. Example: binary term, resource struct
    AllocFail,
    /// Encodes the string into an atom and returns it from the NIF.
    Atom(&'static str),
}
impl NifEncoder for NifError {
    fn encode<'a>(&self, env: &'a NifEnv) -> NifTerm<'a> {
        NifTerm::new(env, match *self {
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

/// NifTerm is used to represent all erlang terms. Terms are always lifetime limited by a NifEnv.
///
/// NifTerm is cloneable and copyable, but it can not exist outside of the lifetime of the NifEnv
/// that owns it.
#[derive(Clone, Copy)]
pub struct NifTerm<'a> {
    term: NIF_TERM,
    env: &'a NifEnv,
}
impl<'a> NifTerm<'a> {
    pub fn new(env: &'a NifEnv, inner: NIF_TERM) -> Self {
        NifTerm {
            term: inner,
            env: env,
        }
    }
    /// This extracts the raw term pointer. It is usually used in order to obtain a type that can
    /// be passed to calls into the erlang vm.
    pub fn as_c_arg(&self) -> NIF_TERM {
        self.term
    }

    pub fn get_env(&self) -> &'a NifEnv {
        self.env
    }

    /// This will coerce the NifTerm into the given NifEnv without providing any checks
    /// or other operations. This is unsafe as it allows you to make a NifTerm usable
    /// on an env other then the one that owns it.
    /// 
    /// The one case where this is acceptable to use is when we already know the NifEnv
    /// is the same, but we need to coerce the lifetime.
    pub unsafe fn env_cast<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        NifTerm::new(env, self.as_c_arg())
    }

    /// Returns a representation of self in the given NifEnv.
    ///
    /// If the term is already is in the provided env, it will be directly returned. Otherwise
    /// the term will be copied over.
    pub fn in_env<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        if self.get_env() == env {
            unsafe { self.env_cast(env) }
        } else {
            NifTerm::new(env, wrapper::copy_term(env.as_c_arg(), self.as_c_arg()))
        }
    }

    pub fn get_type(&self) -> dynamic::TermType {
        dynamic::get_type(self)
    }

    /// Decodes the NifTerm into type T.
    /// 
    /// This should be used as the primary method of extracting the value from a NifTerm.
    /// 
    /// # Examples
    ///
    /// ```ignore
    /// let term: NifTerm = ...;
    /// let number: i32 = try!(term.decode());
    /// ```
    pub fn decode<T>(self) -> NifResult<T> where T: NifDecoder<'a> {
        NifDecoder::decode(self)
    }
}
