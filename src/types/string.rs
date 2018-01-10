use ::{ Term, Env, Encoder, Decoder, NifResult, NifError };
use super::binary::{ NifBinary, OwnedNifBinary };

impl<'a> Decoder<'a> for String {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let string: &str = try!(Decoder::decode(term));
        Ok(string.to_string())
    }
}
impl<'a> Decoder<'a> for &'a str {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let binary = try!(NifBinary::from_term(term));
        match ::std::str::from_utf8(binary.as_slice()) {
            Ok(string) => Ok(string),
            Err(_) => Err(NifError::BadArg),
        }
    }
}

use std::io::Write;

impl Encoder for str {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let str_len = self.len();
        let mut bin = match OwnedNifBinary::new(str_len) {
            Some(bin) => bin,
            None => panic!("binary term allocation fail"),
        };
        bin.as_mut_slice().write(self.as_bytes()).expect("memory copy of string failed");
        bin.release(env).to_term(env)
    }
}

impl Encoder for String {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.as_str().encode(env)
    }
}
