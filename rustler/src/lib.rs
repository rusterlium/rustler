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
//! # Getting Started
//! There is a [`:rustler`](https://hex.pm/packages/rustler) package on hex.pm that provides
//! functionality which makes working with Rustler easier, including project generators, an
//! automatic NIF compiler for Mix, and utilities for loading the compiled NIF.
//!
//! For more information about this, see [the documentation for
//! rustler](https://hexdocs.pm/rustler).

#[doc(hidden)]
pub mod wrapper;

#[doc(hidden)]
pub mod codegen_runtime;

mod alloc;

#[macro_use]
pub mod types;

mod term;
mod wrapped_types;

pub use crate::term::Term;
pub use crate::types::{
    Atom, Binary, Decoder, Encoder, ErlOption, LocalPid, NewBinary, OwnedBinary,
};

pub use crate::wrapped_types::{ListIterator, Map, MapIterator, Reference, Tuple, Wrapper};

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

pub type NifResult<T> = Result<T, Error>;

pub use rustler_codegen::{
    init, nif, resource_impl, NifException, NifMap, NifRecord, NifStruct, NifTaggedEnum, NifTuple,
    NifUnitEnum, NifUntaggedEnum,
};

#[cfg(feature = "serde")]
pub mod serde;

#[cfg(feature = "serde")]
pub use crate::serde::SerdeTerm;

pub mod sys;
