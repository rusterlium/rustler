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
//! Rustler has a Mix archive that provides several commands which makes working with Rustler
//! easier, including project generators, a tool for validating your Rust and Erlang environment,
//! as well as build utilities.
//!
//! As long as you have Elixir and Mix installed, you can install the archive by doing `mix
//! archive.install https://github.com/hansihe/rustler_archives/raw/master/rustler_installer.ez`.
//! When the archive is installed, you can generate a new Rustler project by doing `mix rustler.new
//! <path>`, and following the instructions.
//!
//! # Integrating with a mix project
//! For projects using mix, there is a [helper package on hex](https://hex.pm/packages/rustler).
//! This package contains a mix compiler that automates environment sanity checks, crate compilation
//! and nif loading. If you are using mix, this package is highly recommended, as it will probably
//! make your life easier.
//!
//! To enable automatic compilation of your Rust NIFs do the following:
//!
//! 1. Add the [rustler package](https://hex.pm/packages/rustler) to the dependencies section of
//!    your mix.exs.
//! 2. Add the `:rustler` compiler to the `compilers` list in your project function. If no 
//!    `compilers` key exists, you should add this: `compilers: [:rustler] ++ Mix.compilers`.
//! 3. To actually make a crate compile, you should add a `rustler_crates: ["CRATE_PATH"]`
//!    key to your project function. `CRATE_PATH` should be a relative path to the directory
//!    containing the `Cargo.toml` from the top directory of your mix project.
//!
//! When finished, your mix.exs should look something like this (some parts omitted):
//!
//! ```elixir
//! defmodule YourProject.Mixfile do
//!   use Mix.Project
//! 
//!   def project do
//!     [app: :your_project,
//!      [...]
//!      compilers: [:rustler] ++ Mix.compilers,
//!      rustler_crates: ["."],
//!      deps: deps]
//!   end
//!
//!   [...]
//! 
//!   defp deps do
//!     [{:rustler, "~> 0.0.7"}]
//!   end
//! end
//! ```
//!
//! You should then try to do `mix compile`. If there are any more issues with your environment,
//! the rustler compiler plugin should give you instructions on how to fix them.
//!
//! ### Loading the NIF
//! Loading a Rustler NIF is done in almost the same way as normal NIFs.
//!
//! The actual loading is done by calling `Rustler.load_nif("<LIBRARY_NAME>")` in the module you
//! want to load the NIF in. This is usually done in the `@on_load` module hook.
//!
//! ```elixir
//! defmodule MyProject.NativeModule do
//!   @on_load :load_nif
//!
//!   defp load_nif do
//!     :ok = Rustler.load_nif("<LIBRARY_NAME>")
//!   end
//!
//!   // When loading a NIF module, dummy clauses for all NIF functions are required.
//!   // NIF dummies usually just error out when called when the NIF is not loaded, as that should
//!   // never normally happen.
//!   def my_native_function(_arg1, _arg2), do: exit(:nif_not_loaded)
//! end
//! ```
//!
//! Note that `<LIBRARY_NAME>` is the name in the `[lib]` section of your `Cargo.toml`.

pub mod wrapper;
use wrapper::nif_interface::{NIF_ENV, NIF_TERM, enif_make_badarg, enif_make_atom_len};
pub use wrapper::nif_interface::size_t;

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
macro_rules! _rustler_export_nifs {
    ($module_name:expr, [$(($fun_export_name:expr, $arity:expr, $rust_fn:ident)),*], $init_function:expr) => { /* ... */ }
}
