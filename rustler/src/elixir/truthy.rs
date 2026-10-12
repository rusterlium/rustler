//!
//! A type to represent a truthy value.
//!
//! In Elixir, a term which does not equal `:false` or `:nil` is considered to be
//! truthy. This does not cleanly map to Rust's `bool` type. To distinguish between
//! `bool` and a truthy value, the newtype `Truthy` can be used.
//!

use crate::types::atom;
use crate::{Decoder, Encoder, Env, NifResult, Term};

pub struct Truthy(bool);

pub fn is_truthy(term: Term) -> bool {
    !((term.as_c_arg() == atom::false_().as_c_arg()) || (term.as_c_arg() == atom::nil().as_c_arg()))
}

impl Encoder for Truthy {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.0.encode(env)
    }
}

impl<'a> Decoder<'a> for Truthy {
    fn decode(term: Term<'a>) -> NifResult<Truthy> {
        Ok(Truthy(is_truthy(term)))
    }
}
