use ::{
    NifEnv,
    NifTerm,
    NifResult,
};

#[macro_use]
pub mod atom;
pub mod binary;
pub mod list;
pub mod map;
pub mod primitive;
pub mod string;
pub mod tuple;
pub mod pid;

pub mod elixir_struct;

pub trait NifEncoder {
    fn encode<'a>(&self, env: NifEnv<'a>) -> NifTerm<'a>;
}
pub trait NifDecoder<'a>: Sized+'a {
    fn decode(term: NifTerm<'a>) -> NifResult<Self>;
}

impl<'a> NifEncoder for NifTerm<'a> {
    fn encode<'b>(&self, env: NifEnv<'b>) -> NifTerm<'b> {
        self.in_env(env)
    }
}
impl<'a> NifDecoder<'a> for NifTerm<'a> {
    fn decode(term: NifTerm<'a>) -> NifResult<Self> {
        Ok(term)
    }
}

impl<'a, T> NifEncoder for &'a T where T: NifEncoder {
    fn encode<'c>(&self, env: NifEnv<'c>) -> NifTerm<'c> {
        <T as NifEncoder>::encode(self, env)
    }
}

impl<T> NifEncoder for Option<T> where T: NifEncoder {
    fn encode<'c>(&self, env: NifEnv<'c>) -> NifTerm<'c> {
        match *self {
            Some(ref value) => value.encode(env),
            None => atom::nil().encode(env),
        }
    }
}

impl<T, E> NifEncoder for Result<T, E> where T: NifEncoder, E: NifEncoder {
    fn encode<'c>(&self, env: NifEnv<'c>) -> NifTerm<'c> {
        match *self {
            Ok(ref value) => (atom::ok().encode(env), value.encode(env)).encode(env),
            Err(ref err) => (atom::error().encode(env), err.encode(env)).encode(env),
        }
    }
}
