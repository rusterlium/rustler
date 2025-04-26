use rustler::types::map::MapIterator;
use rustler::types::tuple::make_tuple;
use rustler::{Atom, Encoder, Env, Error, ListIterator, NifResult, Term};

#[rustler::nif]
pub fn sum_map_values(iter: MapIterator) -> NifResult<i64> {
    let res: NifResult<Vec<i64>> = iter.map(|(_key, value)| value.decode::<i64>()).collect();

    let nums = res?;
    Ok(nums.into_iter().sum())
}

#[rustler::nif]
pub fn map_entries<'a>(env: Env<'a>, iter: MapIterator<'a>) -> NifResult<Vec<Term<'a>>> {
    let mut vec = vec![];
    for (key, value) in iter {
        let key_string = key.decode::<String>()?;
        vec.push((key_string, value));
    }

    let erlang_pairs: Vec<Term> = vec
        .into_iter()
        .map(|(key, value)| make_tuple(env, &[key.encode(env), value]))
        .collect();
    Ok(erlang_pairs)
}

#[rustler::nif]
pub fn map_entries_reversed<'a>(env: Env<'a>, iter: MapIterator<'a>) -> NifResult<Vec<Term<'a>>> {
    let mut vec = vec![];
    for (key, value) in iter.rev() {
        let key_string = key.decode::<String>()?;
        vec.push((key_string, value));
    }

    let erlang_pairs: Vec<Term> = vec
        .into_iter()
        .map(|(key, value)| make_tuple(env, &[key.encode(env), value]))
        .collect();
    Ok(erlang_pairs)
}

#[rustler::nif]
pub fn map_from_arrays<'a>(
    env: Env<'a>,
    keys: Vec<Term<'a>>,
    values: Vec<Term<'a>>,
) -> NifResult<Term<'a>> {
    Term::map_from_arrays(env, &keys, &values)
}

#[rustler::nif]
pub fn map_from_pairs<'a>(env: Env<'a>, pairs: ListIterator<'a>) -> NifResult<Term<'a>> {
    let res: Result<Vec<(Term, Term)>, Error> = pairs.map(|x| x.decode()).collect();

    res.and_then(|v| Term::map_from_pairs(env, &v))
}

#[rustler::nif]
pub fn map_generic(
    map: std::collections::HashMap<i64, String>,
) -> std::collections::HashMap<i64, String> {
    map
}

#[rustler::nif]
pub fn map_atom_keys(
    map: std::collections::HashMap<Atom, String>,
) -> std::collections::HashMap<Atom, String> {
    map
}
