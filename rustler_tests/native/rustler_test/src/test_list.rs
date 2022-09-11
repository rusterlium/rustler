use rustler::{Encoder, Env, Error, ListIterator, NifResult, Term};
use rustler::wrapper::list;

#[rustler::nif]
pub fn sum_list(iter: ListIterator) -> NifResult<i64> {
    let res: Result<Vec<i64>, Error> = iter.map(|x| x.decode::<i64>()).collect();

    match res {
        Ok(result) => Ok(result.iter().sum::<i64>()),
        Err(err) => Err(err),
    }
}

#[rustler::nif]
pub fn make_list() -> Vec<usize> {
    vec![1, 2, 3]
}


#[rustler::nif]
pub fn make_list_from_iter(env: Env) -> Term {
  let mut iter = [1, 2, 3].iter().map(|i| (i + 1).encode(env).as_c_arg());
  unsafe { Term::new(env, list::make_list_from_iter(env.as_c_arg(), &mut iter)) }
}

#[rustler::nif]
pub fn make_list_from_end(env: Env) -> Term {
  let mut iter = [1, 2, 3].iter().map(|i| (i + 1).encode(env).as_c_arg());
  unsafe { Term::new(env, list::make_list_from_end(env.as_c_arg(), &mut iter)) }
}
