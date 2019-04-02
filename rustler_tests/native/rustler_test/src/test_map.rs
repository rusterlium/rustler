use rustler::types::map::MapIterator;
use rustler::types::tuple::make_tuple;
use rustler::{Encoder, Env, NifResult, Term};

pub fn sum_map_values<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<i64> {
    let iter: MapIterator = args[0].decode()?;

    let res: NifResult<Vec<i64>> = iter.map(|(_key, value)| value.decode::<i64>()).collect();

    let nums = res?;
    let total: i64 = nums.into_iter().sum();
    Ok(total)
}

pub fn map_entries_sorted<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let iter: MapIterator = args[0].decode()?;

    let mut vec = vec![];
    for (key, value) in iter {
        let key_string = key.decode::<String>()?;
        vec.push((key_string, value));
    }

    vec.sort_by_key(|pair| pair.0.clone());
    let erlang_pairs: Vec<Term> = vec
        .into_iter()
        .map(|(key, value)| make_tuple(env, &[key.encode(env), value]))
        .collect();
    Ok(erlang_pairs.encode(env))
}

pub fn map_from_arrays<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let keys: Vec<Term> = args[0].decode()?;
    let values: Vec<Term> = args[1].decode()?;

    Term::map_from_arrays(env, &keys, &values)
}
