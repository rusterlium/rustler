//! Transparent, typed wrappers around raw Erlang terms.
//!
//! These wrappers complement the higher-level APIs in [`crate::types`]. [`Atom`] and
//! [`LocalPid`] implement [`crate::FreeTerm`]; all other wrappers implement
//! [`crate::BoundTerm`] and retain the lifetime of their originating environment.
//! Integers, floats, references, ports, and general pids are conservatively bound: their
//! representation is not guaranteed to be environment-independent.
//!
//! All wrappers implement [`crate::Decoder`]. Decoding checks the term's type and retains
//! its raw representation without copying, returning [`crate::Error::BadArg`] on a type
//! mismatch. Bound wrappers retain the input environment's lifetime; free wrappers can be
//! decoded from any environment and remain valid after that environment is cleared or freed.
//!
//! All wrappers implement [`crate::Encoder`]. Bound wrappers always copy into the destination
//! environment, while free wrappers reuse their raw term without copying. Associating a bound
//! wrapper with an environment without copying via `to_term` remains unsafe because wrappers
//! do not store their originating environment.
//!
//! Type-specific methods live on the wrappers themselves, such as [`Map::get`] and
//! [`List::get_cell`]. They are safe and do not repeat wrapper type checks. Bound operations
//! copy into the supplied environment, and returned terms and iterators are bound to that
//! destination rather than the original environment. Native API failures, missing keys,
//! and improper-list errors are still reported.
//! The corresponding methods on [`crate::Term`] validate and delegate to these wrappers.
//!
//! [`TypedTerm`] represents any supported term as an enum of these wrappers.
//!
//! ```compile_fail
//! use rustler::{Term, terms::Binary};
//!
//! fn escape<'a>(term: Term<'a>) -> Binary<'static> {
//!     term.decode().unwrap()
//! }
//! ```
//!
//! ```compile_fail
//! use rustler::{Env, Term, terms::Binary};
//!
//! fn convert<'a>(binary: Binary<'a>, env: Env<'a>) -> Term<'a> {
//!     binary.to_term(env) // Requires unsafe and the original environment.
//! }
//! ```

macro_rules! bound_term {
    ($name:ident, $check:expr) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        pub struct $name<'a> {
            raw: crate::sys::ErlNifTerm,
            lifetime: std::marker::PhantomData<&'a ()>,
        }

        impl<'a> $name<'a> {
            /// Check the term's type and retain its environment lifetime.
            pub fn from_term(term: crate::Term<'a>) -> crate::NifResult<Self> {
                <Self as crate::BoundTerm<'a>>::from_term(term)
            }

            /// Return the raw Erlang term.
            pub fn as_c_arg(&self) -> crate::sys::ErlNifTerm {
                self.raw
            }

            /// Associate this wrapper with its original environment.
            ///
            /// # Safety
            ///
            /// `env` must own this term. Sharing a lifetime is not sufficient.
            pub unsafe fn to_term(&self, env: crate::Env<'a>) -> crate::Term<'a> {
                crate::BoundTerm::to_term(self, env)
            }
        }

        unsafe impl<'a> crate::BoundTerm<'a> for $name<'a> {
            fn as_c_arg(&self) -> crate::sys::ErlNifTerm {
                self.raw
            }

            fn from_term(term: crate::Term<'a>) -> crate::NifResult<Self> {
                if ($check)(term) {
                    Ok(unsafe { Self::from_raw(term.as_c_arg(), term.get_env()) })
                } else {
                    Err(crate::Error::BadArg)
                }
            }

            unsafe fn from_raw(raw: crate::sys::ErlNifTerm, _env: crate::Env<'a>) -> Self {
                Self {
                    raw,
                    lifetime: std::marker::PhantomData,
                }
            }
        }

        impl<'a> crate::Decoder<'a> for $name<'a> {
            fn decode(term: crate::Term<'a>) -> crate::NifResult<Self> {
                Self::from_term(term)
            }
        }

        impl crate::Encoder for $name<'_> {
            fn encode<'a>(&self, env: crate::Env<'a>) -> crate::Term<'a> {
                unsafe {
                    crate::Term::new(env, crate::sys::enif_make_copy(env.as_c_arg(), self.raw))
                }
            }
        }

        impl<'a> TryFrom<crate::Term<'a>> for $name<'a> {
            type Error = crate::Error;

            fn try_from(term: crate::Term<'a>) -> Result<Self, Self::Error> {
                Self::from_term(term)
            }
        }
    };
}

macro_rules! free_term {
    ($name:ident, $check:expr) => {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        pub struct $name {
            raw: crate::sys::ErlNifTerm,
        }

        impl $name {
            /// Check the term's type without retaining its environment.
            pub fn from_term(term: crate::Term<'_>) -> crate::NifResult<Self> {
                <Self as crate::FreeTerm>::from_term(term)
            }

            /// Return the raw Erlang term.
            pub fn as_c_arg(&self) -> crate::sys::ErlNifTerm {
                self.raw
            }

            /// Use this term in any environment without copying it.
            pub fn to_term<'a>(&self, env: crate::Env<'a>) -> crate::Term<'a> {
                crate::FreeTerm::in_env(self, env)
            }
        }

        unsafe impl crate::FreeTerm for $name {
            fn as_c_arg(&self) -> crate::sys::ErlNifTerm {
                self.raw
            }

            fn from_term(term: crate::Term<'_>) -> crate::NifResult<Self> {
                if ($check)(term) {
                    Ok(unsafe { Self::from_raw(term.as_c_arg()) })
                } else {
                    Err(crate::Error::BadArg)
                }
            }

            unsafe fn from_raw(raw: crate::sys::ErlNifTerm) -> Self {
                Self { raw }
            }
        }

        impl<'a> crate::Decoder<'a> for $name {
            fn decode(term: crate::Term<'a>) -> crate::NifResult<Self> {
                Self::from_term(term)
            }
        }

        impl crate::Encoder for $name {
            fn encode<'a>(&self, env: crate::Env<'a>) -> crate::Term<'a> {
                crate::FreeTerm::in_env(self, env)
            }
        }

        impl<'a> TryFrom<crate::Term<'a>> for $name {
            type Error = crate::Error;

            fn try_from(term: crate::Term<'a>) -> Result<Self, Self::Error> {
                Self::from_term(term)
            }
        }
    };
}

pub mod atom;
pub mod binary;
#[cfg(feature = "nif_version_2_15")]
pub mod bitstring;
pub mod float;
pub mod fun;
pub mod integer;
pub mod list;
pub mod local_pid;
pub mod map;
pub mod pid;
pub mod port;
pub mod reference;
pub mod tuple;
pub mod typed_term;

pub use atom::Atom;
pub use binary::Binary;
#[cfg(feature = "nif_version_2_15")]
pub use bitstring::Bitstring;
pub use float::Float;
pub use fun::Fun;
pub use integer::Integer;
pub use list::List;
pub use local_pid::LocalPid;
pub use map::Map;
pub use pid::Pid;
pub use port::Port;
pub use reference::Reference;
pub use tuple::Tuple;
pub use typed_term::TypedTerm;
