use ::{
    NifEnv,
    NifTerm,
    NifResult,
};

pub mod atom;
pub mod binary;
pub mod list;
pub mod map;
pub mod primitive;
pub mod string;
pub mod tuple;

pub mod elixir_struct;

pub trait NifEncoder {
    fn encode<'a>(&self, env: &'a NifEnv) -> NifTerm<'a>;
}
pub trait NifDecoder<'a>: Sized+'a {
    fn decode(term: NifTerm<'a>) -> NifResult<Self>;
}

impl<'a> NifEncoder for NifTerm<'a> {
    fn encode<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        self.in_env(env)
    }
}
impl<'a> NifDecoder<'a> for NifTerm<'a> {
    fn decode(term: NifTerm<'a>) -> NifResult<Self> {
        Ok(term)
    }
}
