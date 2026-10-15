#![allow(non_camel_case_types)]
#![allow(clippy::missing_safety_doc)]

//! [Github](https://github.com/rusterlium/rustler)
//! [Example](https://github.com/rusterlium/NifIo)
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
//! Language-specific helpers live in [`elixir`] and [`erlang`], while shared term types live
//! in [`types`].
//!
//! # Getting Started
//! There is a [`:rustler`](https://hex.pm/packages/rustler) package on hex.pm that provides
//! functionality which makes working with Rustler easier, including project generators, an
//! automatic NIF compiler for Mix, and utilities for loading the compiled NIF.
//!
//! For more information about this, see [the documentation for
//! rustler](https://hexdocs.pm/rustler).

#[doc(hidden)]
pub mod codegen_runtime;

mod alloc;
pub(crate) mod traits;
pub use crate::alloc::EnifAllocator;

#[macro_use]
pub mod types;

pub mod elixir;
pub mod erlang;

mod term;

pub use crate::traits::{Decoder, Encoder};

pub use crate::erlang::ErlOption;
pub use crate::term::Term;
pub use crate::types::{
    Atom, Binary, ListIterator, LocalPid, MapIterator, NewBinary, OwnedBinary, Reference,
};

#[cfg(feature = "big_integer")]
pub use crate::types::BigInt;

mod resource;
pub use crate::resource::{Monitor, Resource, ResourceArc, ResourceInitError};

#[doc(hidden)]
pub mod dynamic;
pub use crate::dynamic::TermType;

pub mod schedule;
pub use crate::schedule::SchedulerFlags;
pub mod env;
pub use crate::env::{Env, OwnedEnv};
pub mod thread;
pub use crate::thread::{spawn, JobSpawner, ThreadSpawner};

pub mod error;
pub use crate::error::Error;

pub mod r#return;
pub use crate::r#return::Return;

#[doc(hidden)]
mod nif;
pub use nif::Nif;

/// Conventional result type for NIFs and other fallible operations.
///
/// When used as the return type of a NIF, `NifResult<T>` is NIF control flow,
/// handled via `NifReturnable` rather than encoded as a normal `Result<T, E>`
/// value: `Ok(value)` returns `value` directly, while `Err(err)` raises or
/// returns the error term described by [`Error`].
///
/// To return `{:ok, value} | {:error, reason}` tuples to Elixir, return a
/// plain `Result<T, E: Encoder>` (or encode the tuples explicitly) instead.
pub type NifResult<T> = Result<T, Error>;

#[macro_export]
macro_rules! term_map {
    ($env:expr, { $($key:expr => $value:expr),* $(,)? }) => {{
        $crate::Term::map_from_term_arrays(
            $env,
            &[$($crate::Encoder::encode(&$key, $env)),*],
            &[$($crate::Encoder::encode(&$value, $env)),*],
        )
        .expect("failed to create map")
    }};
}

pub use rustler_codegen::{
    init, nif, resource_impl, NifException, NifMap, NifRecord, NifStruct, NifTaggedEnum, NifTuple,
    NifUnitEnum, NifUntaggedEnum,
};

#[cfg(feature = "serde")]
pub mod serde;

#[cfg(feature = "serde")]
pub use crate::serde::SerdeTerm;

pub mod sys;

#[cfg(not(panic = "unwind"))]
compile_error!("Rustler relies on unwinding (panic = \"unwind\") for callback safety");
