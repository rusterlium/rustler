use std::ops::Deref;

use crate::{Decoder, Encoder, Env, Error, NifResult, Term};

use crate::sys::enif_make_ref;

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Reference<'a>(Term<'a>);

impl<'a> Deref for Reference<'a> {
    type Target = Term<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> From<Reference<'a>> for Term<'a> {
    fn from(term: Reference<'a>) -> Self {
        term.0
    }
}

impl<'a> TryFrom<Term<'a>> for Reference<'a> {
    type Error = Error;

    fn try_from(term: Term<'a>) -> Result<Self, Self::Error> {
        if term.is_ref() {
            Ok(Reference(term))
        } else {
            Err(Error::BadArg)
        }
    }
}

impl<'a> Decoder<'a> for Reference<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        term.try_into()
    }
}

impl<'a> Encoder for Reference<'a> {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.0.encode(env)
    }
}

impl<'a> Env<'a> {
    pub fn make_ref(self) -> Reference<'a> {
        unsafe { Reference(Term::new(self, enif_make_ref(self.as_c_arg()))) }
    }
}
