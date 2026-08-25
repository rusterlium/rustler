use crate::{Binary, Decoder, Encoder, Env, Error, NewBinary, NifResult, Term};

impl<'a> Decoder<'a> for String {
    #[inline]
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let string: &str = Decoder::decode(term)?;
        Ok(string.to_string())
    }
}

impl<'a> Decoder<'a> for &'a str {
    #[inline]
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let binary = Binary::from_term(term)?;
        match ::std::str::from_utf8(binary.as_slice()) {
            Ok(string) => Ok(string),
            Err(_) => Err(Error::BadArg),
        }
    }
}

impl Encoder for &str {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        (*self).encode(env)
    }
}

impl Encoder for str {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        NewBinary::from_slice(env, self).into()
    }
}

impl Encoder for String {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.as_str().encode(env)
    }
}
