use rustler::{Encoder, Env, Error, ListIterator, Map, MapIterator, NifResult, Term, Tuple};

#[rustler::nif]
pub fn sum_map_values(iter: MapIterator) -> NifResult<i64> {
    let res: NifResult<Vec<i64>> = iter.map(|(_key, value)| value.decode::<i64>()).collect();

    let nums = res?;
    Ok(nums.into_iter().sum())
}

#[rustler::nif]
pub fn map_entries<'a>(env: Env<'a>, iter: MapIterator<'a>) -> NifResult<Vec<Tuple<'a>>> {
    let mut vec = vec![];
    for (key, value) in iter {
        let key_string = key.decode::<String>()?;
        vec.push((key_string, value));
    }

    let erlang_pairs: Vec<_> = vec
        .into_iter()
        .map(|(key, value)| env.make_tuple(&[key.encode(env), value]))
        .collect();
    Ok(erlang_pairs)
}

#[rustler::nif]
pub fn map_entries_reversed<'a>(env: Env<'a>, iter: MapIterator<'a>) -> NifResult<Vec<Tuple<'a>>> {
    let mut vec = vec![];
    for (key, value) in iter.rev() {
        let key_string = key.decode::<String>()?;
        vec.push((key_string, value));
    }

    let erlang_pairs: Vec<_> = vec
        .into_iter()
        .map(|(key, value)| env.make_tuple(&[key.encode(env), value]))
        .collect();
    Ok(erlang_pairs)
}

#[rustler::nif]
pub fn map_from_arrays<'a>(
    env: Env<'a>,
    keys: Vec<Term<'a>>,
    values: Vec<Term<'a>>,
) -> NifResult<Map<'a>> {
    env.map_from_arrays(&keys, &values)
}

#[rustler::nif]
pub fn map_from_pairs<'a>(env: Env<'a>, pairs: ListIterator<'a>) -> NifResult<Map<'a>> {
    let res: Result<Vec<(Term, Term)>, Error> = pairs.map(|x| x.decode()).collect();

    res.and_then(|v| env.map_from_pairs(&v))
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
