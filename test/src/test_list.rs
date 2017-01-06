use rustler::{NifEnv, NifTerm, NifError, NifEncoder, NifResult};
use rustler::types::list::NifListIterator;

pub fn sum_list<'a>(env: NifEnv<'a>, args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let iter: NifListIterator = try!(args[0].decode());

    let res: Result<Vec<i64>, NifError> = iter
        .map(|x| x.decode::<i64>())
        .collect();

    match res {
        Ok(result) => Ok(result.iter().fold(0, |acc, &x| acc + x).encode(env)),
        Err(err) => Err(err),
    }
}

pub fn make_list<'a>(env: NifEnv<'a>, _args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let list = vec![1, 2, 3];
    Ok(list.encode(env))
}
