use rustler::{Encoder, Env, Error, Term};
use std::ops::RangeInclusive;

pub fn sum_range<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let range: RangeInclusive<i64> = args[0].decode()?;

    let total: i64 = range.into_iter().sum();
    Ok(total.encode(env))
}
