use rustler::{Env, Error as NifError, NifResult, Term, Binary};
use serde_bytes::Bytes;
use rustler::serde::{to_term, Deserializer, Serializer};
use serde_transcode::transcode;

#[nif]
pub fn decode_json<'a>(env: Env<'a>, json_bytes: Binary) -> NifResult<Term<'a>> {
    decode(env, &json_bytes)
}

#[nif(schedule = "DirtyCpu")]
pub fn decode_json_dirty<'a>(env: Env<'a>, json_bytes: Binary) -> NifResult<Term<'a>> {
    decode(env, &json_bytes)
}

#[inline]
/// Deserializes a JSON string into an Elixir term.
fn decode<'a>(env: Env<'a>, json_bytes: &[u8]) -> NifResult<Term<'a>> {
    let mut de = serde_json::Deserializer::from_slice(json_bytes);
    let ser = Serializer::from(env);
    transcode(&mut de, ser).map_err(|err| err.into())
}

#[nif]
pub fn encode_json_compact<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    encode_compact(env, arg)
}

#[nif(schedule = "DirtyCpu")]
pub fn encode_json_compact_dirty<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    encode_compact(env, arg)
}

#[inline]
/// Serializes an Elixir term into a compact JSON string.
pub fn encode_compact<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    let de = Deserializer::from(arg);
    let mut ser_vec = Vec::new();
    let mut ser = serde_json::Serializer::new(&mut ser_vec);
    transcode(de, &mut ser).or(Err(NifError::RaiseAtom("transcode error")))?;
    to_term(env, Bytes::new(&ser_vec)).map_err(|err| err.into())
}

#[nif]
pub fn encode_json_pretty<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    encode_pretty(env, arg)
}

#[nif(schedule = "DirtyCpu")]
pub fn encode_json_pretty_dirty<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    encode_pretty(env, arg)
}

#[inline]
/// Serializes an Elixir term into a pretty-printed JSON string.
pub fn encode_pretty<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    let de = Deserializer::from(arg);
    let mut ser_vec = Vec::new();
    let mut ser = serde_json::Serializer::pretty(&mut ser_vec);
    transcode(de, &mut ser).or(Err(NifError::RaiseAtom("transcode_error")))?;
    to_term(env, Bytes::new(&ser_vec)).map_err(|err| err.into())
}
