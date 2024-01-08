pub mod atoms;
mod de;
mod error;
mod ser;
mod util;

pub use de::{from_term, Deserializer};
pub use error::Error;
pub use ser::{to_term, Serializer};

use crate::{Decoder, Encoder, Env, NifResult, Term};

/* impl<T: ?Sized> crate::Encoder for T where T: serde::Serialize {
    fn encode<'a>(&self, env: crate::Env<'a>) -> crate::Term<'a> {
        to_term(env, self).unwrap()
    }
} */

pub struct SerdeTerm<T>(pub T);

impl<T: serde::Serialize> Encoder for SerdeTerm<T> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        let ser = crate::serde::Serializer::from(env);
        self.0.serialize(ser).unwrap()
    }
}

impl<'a, T: serde::Deserialize<'a> + 'a> Decoder<'a> for SerdeTerm<T> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if let Ok(decoded) = from_term(term) {
            Ok(SerdeTerm(decoded))
        } else {
            panic!("invalid conversion")
        }
    }
}
