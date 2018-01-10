use rustler::{NifEnv, Term, Encoder, NifResult};
use rustler::types::map::NifMapIterator;
use rustler::types::tuple::make_tuple;

pub fn sum_map_values<'a>(env: NifEnv<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let iter: NifMapIterator = args[0].decode()?;

    let res: NifResult<Vec<i64>> = iter
        .map(|(_key, value)| value.decode::<i64>())
        .collect();

    let nums = res?;
    let total: i64 = nums.into_iter().sum();
    Ok(total.encode(env))
}

pub fn map_entries_sorted<'a>(env: NifEnv<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let iter: NifMapIterator = args[0].decode()?;

    let mut vec = vec![];
    for (key, value) in iter {
        let key_string = key.decode::<String>()?;
        vec.push((key_string, value));
    }

    vec.sort_by_key(|pair| pair.0.clone());
    let erlang_pairs: Vec<Term> =
        vec.into_iter()
        .map(|(key, value)| make_tuple(env, &[key.encode(env), value]))
        .collect();
    Ok(erlang_pairs.encode(env))
}
