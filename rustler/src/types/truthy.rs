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

impl Encoder for Truthy {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.0.encode(env)
    }
}

impl<'a> Decoder<'a> for Truthy {
    fn decode(term: Term<'a>) -> NifResult<Truthy> {
        Ok(Truthy(atom::is_truthy(term)))
    }
}
