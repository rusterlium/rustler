use crate::codegen_runtime::{NifReturnable, NifReturned};
use crate::{types, Encoder, Env};
use std::fmt;

/// Represents usual errors that can happen in a nif. This enables you
/// to return an error from anywhere, even places where you don't have
/// an Env available.
pub enum Error {
    /// Returned when the NIF has been called with the wrong number or type of
    /// arguments.
    BadArg,
    /// Encodes the string into an atom and returns it from the NIF.
    Atom(&'static str),
    RaiseAtom(&'static str),
    RaiseTerm(Box<dyn Encoder>),
}

unsafe impl NifReturnable for crate::error::Error {
    unsafe fn as_returned(self, env: Env) -> NifReturned {
        match self {
            Error::BadArg => NifReturned::BadArg,
            Error::Atom(atom_str) => {
                let atom = types::atom::Atom::from_str(env, atom_str)
                    .ok()
                    .expect("Error::Atom: bad atom")
                    .to_term(env);
                NifReturned::Term(atom.as_c_arg())
            }
            Error::RaiseAtom(atom_str) => {
                let atom = types::atom::Atom::from_str(env, atom_str)
                    .ok()
                    .expect("Error::RaiseAtom: bad argument");
                NifReturned::Raise(atom.as_c_arg())
            }
            Error::RaiseTerm(ref term_unencoded) => {
                let term = term_unencoded.encode(env);
                NifReturned::Raise(term.as_c_arg())
            }
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Error::BadArg => write!(fmt, "{{error, badarg}}"),
            Error::Atom(ref s) => write!(fmt, "{{error, {}}}", s),
            Error::RaiseAtom(ref s) => write!(fmt, "throw({})", s),
            Error::RaiseTerm(_) => write!(fmt, "throw(<term>)"),
        }
    }
}