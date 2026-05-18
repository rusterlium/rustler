//! Constants and utilities for conversion between Rust string-likes and Elixir atoms.

use crate::serde::Error;
use crate::{types::atom::Atom, Encoder, Env, Term};

atoms! {
    nil,
    ok,
    error,
    true_ = "true",
    false_ = "false",
    undefined,
    nan,
    inf,
    neg_inf,
    __struct__,
}

/**
 * Attempts to create an atom term from the provided string (if the atom already exists in the atom table). If not, returns a string term.
 */
pub fn str_to_term<'a>(env: Env<'a>, string: &str) -> Term<'a> {
    match Atom::try_from_str(env, string) {
        Ok(term) => term.encode(env),
        Err(_) => string.encode(env),
    }
}

/**
 * Attempts to create a `String` from the term.
 */
pub fn term_to_string(term: &Term) -> Result<String, Error> {
    if term.is_atom() {
        term.atom_to_string().or(Err(Error::InvalidAtom))
    } else {
        Err(Error::InvalidStringable)
    }
}
