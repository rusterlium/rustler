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

#[doc(hidden)]
pub mod wrapper;
use wrapper::nif_interface::{NIF_ENV};
pub use wrapper::nif_interface::size_t;
pub use wrapper::nif_interface::ErlNifTaskFlags;

#[doc(hidden)]
pub mod codegen_runtime;

#[macro_use]
extern crate lazy_static;

mod term;
pub use term::{ NifTerm };
mod types;
pub use types::{ NifEncoder, NifDecoder };
pub mod resource;
pub mod binary;
pub mod tuple;
pub mod map;
pub mod list;
pub mod atom;

pub mod ex_struct;
pub mod dynamic;

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

/// Represents usual errors that can happen in a nif. This enables you
/// to return an error from anywhere, even places where you don't have
/// an NifEnv availible.
pub enum NifError {
    /// Returned when the NIF has been called with the wrong number or type of
    /// arguments.
    BadArg,
    /// Encodes the string into an atom and returns it from the NIF.
    Atom(&'static str),
    RaiseAtom(&'static str),
    RaiseTerm(Box<NifEncoder>),
}

impl NifError {

    /// Unsafe because it allows you to do things that are non-rusty.
    /// (Like raising an exception from anywhere, without having it
    /// be the return value)
    unsafe fn encode<'a>(self, env: &'a NifEnv) -> NifTerm<'a> {
        match self {
            NifError::BadArg =>{
                let exception = wrapper::exception::raise_badarg(env.as_c_arg());
                NifTerm::new(env, exception)
            },
            NifError::Atom(atom_str) => {
                atom::get_atom_init(atom_str).to_term(env)
            },
            NifError::RaiseAtom(atom_str) => {
                let atom = atom::get_atom_init(atom_str).to_term(env);
                let exception = wrapper::exception::raise_exception(
                    env.as_c_arg(),
                    atom.as_c_arg());
                NifTerm::new(env, exception)
            },
            NifError::RaiseTerm(ref term_unencoded) => {
                let term = term_unencoded.encode(env);
                let exception = wrapper::exception::raise_exception(
                    env.as_c_arg(),
                    term.as_c_arg());
                NifTerm::new(env, exception)
            },
        }
    }

}
