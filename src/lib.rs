#![feature(recover)]
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
//! When starting a project with Rustler, the first thing we need is Rust itself. Because this
//! library uses compiler plugins for code generation, we need the nightly version of the rust
//! compiler.  Rust nightly can be installed by following [the instructions on the install
//! page](https://www.rust-lang.org/downloads.html#nightly). When using rust for more than testing,
//! it is recommended to use [multirust](https://github.com/brson/multirust), which takes care of
//! downloading, installing, updating and switching between different versions of rust.
//! 
//! ## Setting up the project
//! Creating and managing a rust project is done with the `cargo` command line utility. Creating a
//! new project is done with `cargo new <project_name>`. This will create a new folder with the
//! project name, which contains the initial scaffold for a rust project.
//!
//! Before we start writing rust code for our NIFs, we need to add the dependencies to the
//! `Cargo.toml` file, as well as specify that we want to generate a dynamically linked library for
//! the BEAM to load. Your `Cargo.toml` file should something like this when done:
//!
//! ```
//! [package]
//! name = "<project_name>"
//! version = "0.1.0"
//! authors = ["<you>"]
//! 
//! [lib]
//! name = "<library name>"
//! path = "src/lib.rs"
//! crate_type = ["dylib"]
//! 
//! [dependencies]
//! rustler = ">=0.5"
//! rustler_codegen = ">=0.5"
//! ```
//! 
//! At this point we can try running `cargo test` to verify that our project compiles, and to run
//! the automatically generated test.
//!
//! ## Writing our NIF
//! As we specified in the `Cargo.toml` file, the entry point to our NIF library is located at
//! `src/lib.rs`. This file was automatically generated when we created the project. By default
//! this file contains one test. Feel free to delete the test for now.
//!
//! To import the rustler library, add these lines to the top of the `lib.rs` file:
//! 
//! ```
//! #![feature(plugin)]
//! #![plugin(rustler_codegen)]
//! 
//! extern crate rustler;
//! use rustler::{NifEnv, NifTerm, NifError, NifResult, NifDecoder, NifEncoder};
//! ```
//!
//! To create a basic NIF, we need to add two more things to our `lib.rs` file, a function to
//! execute, and the nif export. 
//!
//! ```
//! rustler_export_nifs!(
//!     "Elixir.OurNifModule",
//!     [("square", 1, square)],
//!     None
//! );
//!
//! fn square<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
//!     Err(NifError::BadArg)
//! }
//! ```
//! 
//! The first parameter in the `rustler_export_nifs!` macro is the name of the module we want to
//! register our NIF in. Since all Elixir modules are prefixed with `Elixir`, this will register
//! the nif library in the `OurNifModule` Elixir module.
//! 
//! The second argument is a list of tuples representing the functions we want to export. Each
//! tuple consists of 3 elements, the name of the function as exposed to Erlang, the arity of our
//! function, and the identifier of the function we define below.
//! 
//! The third argument is a function that will be executed when the NIF library is loaded. This is
//! optional, and we will pass None fow now.
//!
//! We also define the `square` function itself. The function takes two arguments, and returns a
//! result. The first argument is the nif environment, which represents the process the NIF is
//! called in. The second argument is a list of terms. This list will always be of the same length
//! as the arity we specified in the `rustler_export_nifs!` entry.
//!
//! The return type is either a NifTerm or a NifError. Right now we simply return a BadArg error.
//!
//! ## Running our NIF
//! At this point our project should successfully compile with the `cargo build` command, and
//! output a dynamically linked library that can be loaded by the BEAM.
//!
//! To test our NIF, we will write a simple `exs` script that loads and runs our NIF, we will name
//! this file `run.exs`, and we will place it in our project root directory.
//!
//! ```elixir
//! defmodule OurNifModule do
//!     @on_load {:init, 0}
//!
//!     def init do
//!         path = :filelib.wildcard('target/{debug,release}/lib<library_name>.*') |> hd 
//!             |> :filename.rootname
//!         :ok = :erlang.load_nif(path, 0)
//!         :ok
//!     end
//!     
//!     def square(_a), do: exit(:nif_not_loaded)
//! end
//! 
//! IO.inspect OurNifModule.square(12)
//! ```
//!
//! The Elixir code for running the NIF is very simple. We specify that we want our init function
//! to run when the module is first loaded by using the @on_load attribute. In our init function we
//! first find the path of the dynalically loaded library we genrated earlier when running `cargo
//! build`. We then call `:erlang.load_nif/2 with our path and some optional data.
//!
//! If we try running it we will see that the OurNifModule.square function returns badarg. This is
//! correct, as that is the error we returned from our rust function.
//!
//! ## Making our NIF useful
//! Our nif now runs, but it doesn't really do anything useful yet. In order to do anything useful,
//! we need to be able to accept input and produce output. This is generally done through the
//! [NifEncoder](trait.NifEncoder.html) and [NifDecoder](trait.NifDecoder.html) traits.
//!
//! Let's try to extract number from the args. As rust is very close to the metal, and uses
//! native types, while erlangs type system is a bit more lenient, we need to inform rust what type
//! we want to extract from the term. If we type annotate the variable we put the decoded value
//! into, the rust compiler is smart enough to propagate the type information, and everything else
//! is handled for us.
//!
//! ```
//! fn square<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
//!     let number: i64 = NifDecoder::decode(args[0], env);
//!     Ok(NifEncoder::encode(number * number))
//! }
//! ```

pub mod wrapper;
use wrapper::nif_interface::{NIF_ENV, NIF_TERM, enif_make_badarg, enif_make_atom_len};

#[macro_use]
extern crate lazy_static;

extern crate libc;

use std::marker::PhantomData;

mod types;
pub use self::types::{ NifEncoder, NifDecoder };
pub mod resource;
pub mod binary;
pub mod tuple;
pub mod map;
pub mod atom;
pub mod codegen_runtime;
pub mod ex_struct;

pub type NifResult<T> = Result<T, NifError>;

/// On each NIF call, a NifEnv is passed in. The NifEnv is used for most operations that involve
/// communicating with the BEAM, like decoding and encoding terms.
///
/// There is no way to allocate a NifEnv at the moment, but this may be possible in the future.
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
                                            name.len() as libc::size_t) },
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
    env_life: PhantomData<&'a NifEnv>,
}
impl<'a> NifTerm<'a> {
    /// Constructs a new term from the raw term pointer `inner`, and associated with the given
    /// lifetime.
    ///
    /// # Safety
    /// This function is unsafe as it allows you to create NifTerms that outlive the NifEnv that
    /// owns it. When using this caution must be taken that an appropriate lifetime is associated
    /// with it.
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
    /// This extracts the raw term pointer. It is usually used in order to obtain a type that can
    /// be passed to calls into the erlang vm.
    pub fn as_c_arg(&self) -> NIF_TERM {
        self.term
    }
}

/// Exports a given list of functions to a Erlang module.
///
/// This should be called exactly once in every NIF library. It will wrap and export the given rust
/// functions into the Erlang module.
/// 
/// The first argument is a string specifying what Erlang/Elixir module you want the function
/// exported into. In Erlang this will simply be the atom you named your module. In Elixir, all
/// modules are prefixed with `Elixir.<module path>`
/// 
/// The second argument is a list of 3-tuples. Each tuple contains information on a single exported
/// NIF function. The first tuple item is the name you want to export the function into, the second
/// is the arity (number of arguments) of the exported function. The third argument is a
/// indentifier of a rust function. This is where your actual NIF will be implemented.
///
/// The third argument is an `Option<fn(env: &NifEnv, load_info: NifTerm) -> bool>`. If this is
/// `Some`, the function will execute when the NIF is first loaded by the BEAM.
///
/// WARNING: Only a stub, actual macro defined in compiler plugin.
#[macro_export]
macro_rules! rustler_export_nifs {
    ($module_name:expr, [$(($fun_export_name:expr, $arity:expr, $rust_fn:ident)),*], $init_function:expr) => { /* ... */ }
}
