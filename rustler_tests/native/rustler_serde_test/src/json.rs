use rustler::{Env, Error as NifError, nif, SerdeTerm, NifResult, Term};
use serde_bytes::Bytes;
use rustler::serde::{to_term, Deserializer, Serializer};
use serde_transcode::transcode;

#[nif]
/// Deserializes a JSON string into an Elixir term.
pub fn decode_json<'a>(env: Env<'a>, SerdeTerm(json_bytes): SerdeTerm<&[u8]>) -> NifResult<Term<'a>> {
    let mut de = serde_json::Deserializer::from_slice(json_bytes);
    let ser = Serializer::from(env);
    transcode(&mut de, ser).map_err(|err| err.into())
}

#[inline]
/// Serializes an Elixir term into a compact JSON string.
pub fn encode_compact<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let de = Deserializer::from(args[0]);
    let mut ser_vec = Vec::new();
    let mut ser = serde_json::Serializer::new(&mut ser_vec);
    transcode(de, &mut ser).or(Err(NifError::RaiseAtom("transcode error")))?;
    to_term(env, Bytes::new(&ser_vec)).map_err(|err| err.into())
}

#[inline]
/// Serializes an Elixir term into a pretty-printed JSON string.
pub fn encode_pretty<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let de = Deserializer::from(args[0]);
    let mut ser_vec = Vec::new();
    let mut ser = serde_json::Serializer::pretty(&mut ser_vec);
    transcode(de, &mut ser).or(Err(NifError::RaiseAtom("transcode error")))?;
    to_term(env, Bytes::new(&ser_vec)).map_err(|err| err.into())
}
