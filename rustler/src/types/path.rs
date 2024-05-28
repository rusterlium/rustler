use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::path::{Path, PathBuf};

impl Encoder for Path {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.as_os_str().to_str().encode(env)
    }
}

impl Encoder for PathBuf {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.as_path().encode(env)
    }
}

impl<'a> Decoder<'a> for &'a Path {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let bin = term.decode_as_binary().or(Err(Error::BadArg))?;
        let s = std::str::from_utf8(bin.as_slice()).or(Err(Error::BadArg))?;
        Ok(Path::new(s))
    }
}

impl<'a> Decoder<'a> for PathBuf {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let s: &str = term.decode()?;
        Ok(PathBuf::from(s))
    }
}
