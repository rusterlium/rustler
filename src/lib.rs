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

#[macro_use(enif_snprintf)]
extern crate erlang_nif_sys;

use std::marker::PhantomData;

mod wrapper;
use wrapper::nif_interface::NIF_ENV;

#[doc(hidden)]
pub mod codegen_runtime;

#[macro_use]
extern crate lazy_static;

#[macro_use]
pub mod types;

mod term;

pub use term::{ Term };
pub use types::{Encoder, Decoder};
pub use wrapper::nif_interface::ErlNifTaskFlags;
pub mod resource;

#[doc(hidden)]
pub mod dynamic;
pub use dynamic::TermType;

pub mod schedule;
pub mod env;
pub mod thread;

mod export;

pub type NifResult<T> = Result<T, NifError>;


/// Private type system hack to help ensure that each environment exposed to safe Rust code is
/// given a different lifetime. The size of this type is zero, so it costs nothing at run time. Its
/// purpose is to make `Env<'a>` and `Term<'a>` *invariant* w.r.t. `'a`, so that Rust won't
/// auto-convert a `Env<'a>` to a `Env<'b>`.
type EnvId<'a> = PhantomData<*mut &'a u8>;

/// On each NIF call, a Env is passed in. The Env is used for most operations that involve
/// communicating with the BEAM, like decoding and encoding terms.
///
/// There is no way to allocate a Env at the moment, but this may be possible in the future.
#[derive(Clone, Copy)]
pub struct Env<'a> {
    env: NIF_ENV,
    id: EnvId<'a>
}

/// Two environments are equal if they're the same `NIF_ENV` value.
///
/// A `Env<'a>` is equal to a `Env<'b>` iff `'a` and `'b` are the same lifetime.
impl<'a, 'b> PartialEq<Env<'b>> for Env<'a> {
    fn eq(&self, other: &Env<'b>) -> bool {
        self.env == other.env
    }
}

impl<'a> Env<'a> {
    /// Create a new Env. For the `_lifetime_marker` argument, pass a
    /// reference to any local variable that has its own lifetime, different
    /// from all other `Env` values. The purpose of the argument is to make
    /// it easier to know for sure that the `Env` you're creating has a
    /// unique lifetime (i.e. that you're following the most important safety
    /// rule of Rustler).
    ///
    /// # Unsafe
    /// Don't create multiple `Env`s with the same lifetime.
    unsafe fn new<T>(_lifetime_marker: &'a T, env: NIF_ENV) -> Env<'a> {
        Env {
            env: env,
            id: PhantomData
        }
    }

    pub fn as_c_arg(&self) -> NIF_ENV {
        self.env
    }

    /// Convenience method for building a tuple `{error, Reason}`.
    pub fn error_tuple<T>(self, reason: T) -> Term<'a>
        where T: Encoder
    {
        let error = types::atom::error().to_term(self);
        (error, reason).encode(self)
    }
}

/// Represents usual errors that can happen in a nif. This enables you
/// to return an error from anywhere, even places where you don't have
/// an Env availible.
pub enum NifError {
    /// Returned when the NIF has been called with the wrong number or type of
    /// arguments.
    BadArg,
    /// Encodes the string into an atom and returns it from the NIF.
    Atom(&'static str),
    RaiseAtom(&'static str),
    RaiseTerm(Box<Encoder>),
}

impl NifError {

    /// # Unsafe
    ///
    /// If `self` is a `BadArg`, `RaiseAtom`, or `RaiseTerm` value, then the
    /// term returned from this method must not be used except as the return
    /// value from the calling NIF.
    unsafe fn encode<'a>(self, env: Env<'a>) -> Term<'a> {
        match self {
            NifError::BadArg => {
                let exception = wrapper::exception::raise_badarg(env.as_c_arg());
                Term::new(env, exception)
            },
            NifError::Atom(atom_str) => {
                types::atom::NifAtom::from_str(env, atom_str)
                    .ok().expect("NifError::Atom: bad atom").to_term(env)
            },
            NifError::RaiseAtom(atom_str) => {
                let atom = types::atom::NifAtom::from_str(env, atom_str)
                    .ok().expect("NifError::RaiseAtom: bad argument");
                let exception = wrapper::exception::raise_exception(
                    env.as_c_arg(),
                    atom.as_c_arg());
                Term::new(env, exception)
            },
            NifError::RaiseTerm(ref term_unencoded) => {
                let term = term_unencoded.encode(env);
                let exception = wrapper::exception::raise_exception(
                    env.as_c_arg(),
                    term.as_c_arg());
                Term::new(env, exception)
            },
        }
    }

}
