// When writing your NIF you should edit `lib.rs.in`. This is done
// to support both compilation with and without syntex.

// Enable the rustler_codegen compiler plugin if we are not using
// syntex.
#![cfg_attr(not(feature = "with-syntex"), feature(plugin))]
#![cfg_attr(not(feature = "with-syntex"), plugin(rustler_codegen))]

#[macro_use]
extern crate rustler;

// If we are using syntex, include the expanded file. This is what
// causes bad error messages.
#[cfg(feature = "with-syntex")]
include!(concat!(env!("OUT_DIR"), "/lib.rs"));

// If we are on nightly, we include the file directly.
// The rust compiler will then report errors in that file.
#[cfg(not(feature = "with-syntex"))]
include!("lib.rs.in");
