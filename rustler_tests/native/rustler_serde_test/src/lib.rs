//! Library implementing tests to be called from ExUnit.
//!
//! See `run_ser_test` and `run_de_test` for details about how to use `serde_rustler::Serializer` and `serde_rustler::Deserializer`.

#[macro_use]
extern crate rustler;

mod json;
mod test;
mod types;

use crate::types::Animal;
use rustler::serde::{atoms, Deserializer, Error, Serializer};
use rustler::{Encoder, Env, NifResult, SerdeTerm, Term};

init!("Elixir.SerdeRustlerTests");

/// Implements the README example.
#[nif]
pub fn readme(SerdeTerm(animal): SerdeTerm<Animal>) -> impl Encoder {
    // println!("\n deserialized animal from README example: {:?}", animal);
    SerdeTerm(animal)
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
    env.make_tuple(&[ok_atom_term, term]).into()
}

fn error_tuple<'a>(env: Env<'a>, reason_term: Term<'a>) -> Term<'a> {
    let err_atom_term = atoms::error().encode(env);
    env.make_tuple(&[err_atom_term, reason_term]).into()
}
