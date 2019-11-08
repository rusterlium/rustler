use rustler::{Error, ListIterator, NifResult};

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
