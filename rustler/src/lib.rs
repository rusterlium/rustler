#![deny(warnings)]
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
pub use crate::alloc::EnifAllocator;

#[macro_use]
pub mod types;

mod term;

pub use crate::term::Term;
pub use crate::types::{
    Atom, Binary, Decoder, Encoder, ErlOption, ListIterator, LocalPid, MapIterator, NewBinary,
    OwnedBinary, Reference,
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
pub use crate::thread::{JobSpawner, ThreadSpawner};

pub mod error;
pub use crate::error::Error;

pub mod r#return;
pub use crate::r#return::Return;

#[doc(hidden)]
mod nif;
pub use nif::Nif;

pub type NifResult<T> = Result<T, Error>;

pub use rustler_codegen::{
    init, nif, resource_impl, task, NifException, NifMap, NifRecord, NifStruct, NifTaggedEnum,
    NifTuple, NifUnitEnum, NifUntaggedEnum,
};

#[cfg(feature = "serde")]
pub mod serde;

#[cfg(feature = "serde")]
pub use crate::serde::SerdeTerm;

#[cfg(feature = "async-rt")]
pub mod runtime;

/// Spawn an async task on the global runtime.
///
/// This provides a runtime-agnostic API similar to `tokio::spawn()`.
/// The future is spawned on the global runtime and executed to completion.
///
/// Returns a join handle that can be used to await the result or cancel the task.
///
/// # Example
///
/// ```ignore
/// let handle = rustler::spawn(async {
///     // Your async code
///     process_data().await
/// });
/// ```
///
/// # Panics
///
/// Panics if the runtime fails to spawn the task.
#[cfg(feature = "tokio-rt")]
pub fn spawn<F>(future: F) -> tokio::task::JoinHandle<F::Output>
where
    F: std::future::Future + Send + 'static,
    F::Output: Send + 'static,
{
    runtime::handle().spawn(future)
}

pub mod sys;
