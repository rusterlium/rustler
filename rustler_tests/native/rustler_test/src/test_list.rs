use rustler::types::list::ListIterator;
use rustler::{Encoder, Env, Error, NifResult, Term};

pub fn sum_list<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let iter: ListIterator = args[0].decode()?;

    let res: Result<Vec<i64>, Error> = iter.map(|x| x.decode::<i64>()).collect();

    match res {
        Ok(result) => Ok(result.iter().fold(0, |acc, &x| acc + x).encode(env)),
        Err(err) => Err(err),
    }
}

pub fn make_list<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let list = vec![1, 2, 3];
    Ok(list.encode(env))
}
