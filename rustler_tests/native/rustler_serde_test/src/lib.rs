//! Library implementing tests to be called from ExUnit.
//!
//! See `run_ser_test` and `run_de_test` for details about how to use `serde_rustler::Serializer` and `serde_rustler::Deserializer`.

#[macro_use]
extern crate rustler;

mod json;
mod test;
mod types;

use crate::types::Animal;
use rustler::serde::{atoms, from_term, to_term, Deserializer, Error, Serializer};
use rustler::{types::tuple, Encoder, Env, NifResult, Term};

init! {
    "Elixir.SerdeRustlerTests",
    [
        // json
        json::decode_json,
        json::decode_json_dirty,
        json::encode_json_compact,
        json::encode_json_compact_dirty,
        json::encode_json_pretty,
        json::encode_json_pretty_dirty,

        // tests
        readme,
        test::test,
        transcode,
        transcode_dirty,
    ]
}

/// Implements the README example.
#[nif]
pub fn readme<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    let animal: Animal = from_term(arg)?;
    println!("\n deserialized animal from README example: {:?}", animal);
    to_term(env, animal).map_err(|err| err.into())
}

/// Deserializes anything from an Elixir term and subsequently serializes the result back into an Elixir term, returning it.
pub fn transcode_main<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    tag_tuple(env, || {
        let de = Deserializer::from(arg);
        let ser = Serializer::from(env);
        serde_transcode::transcode(de, ser)
    })
}

#[nif]
pub fn transcode<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    transcode_main(env, arg)
}

#[nif(schedule = "DirtyCpu")]
pub fn transcode_dirty<'a>(env: Env<'a>, arg: Term<'a>) -> NifResult<Term<'a>> {
    transcode_main(env, arg)
}

fn tag_tuple<'a, F>(env: Env<'a>, func: F) -> NifResult<Term<'a>>
where
    F: FnOnce() -> Result<Term<'a>, Error>,
{
    match func() {
        Ok(term) => Ok(ok_tuple(env, term)),
        Err(reason) => {
            let reason_term = reason.to_string().encode(env);
            Ok(error_tuple(env, reason_term))
        }
    }
}

fn ok_tuple<'a>(env: Env<'a>, term: Term<'a>) -> Term<'a> {
    let ok_atom_term = atoms::ok().encode(env);
    tuple::make_tuple(env, &[ok_atom_term, term])
}

fn error_tuple<'a>(env: Env<'a>, reason_term: Term<'a>) -> Term<'a> {
    let err_atom_term = atoms::error().encode(env);
    tuple::make_tuple(env, &[err_atom_term, reason_term])
}
