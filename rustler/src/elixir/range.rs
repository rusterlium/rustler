//! Decoding Elixir ranges into Rust inclusive ranges.

use crate::types::atom;
use crate::{Decoder, Error, NifResult, Term};
use std::ops::RangeInclusive;

impl<'a, T> Decoder<'a> for RangeInclusive<T>
where
    T: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let name = term.map_get(atom::__struct__())?;

        match name.atom_to_string()?.as_ref() {
            "Elixir.Range" => (),
            _ => return Err(Error::BadArg),
        }

        let first = term.map_get(atom::first())?.decode::<T>()?;
        let last = term.map_get(atom::last())?.decode::<T>()?;
        if let Ok(step) = term.map_get(atom::step()) {
            match step.decode::<i64>()? {
                1 => (),
                _ => return Err(Error::BadArg),
            }
        }

        Ok(first..=last)
    }
}
