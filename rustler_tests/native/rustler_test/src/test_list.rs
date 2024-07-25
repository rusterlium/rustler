use rustler::{Error, ListIterator, NifResult};
use std::panic;

#[rustler::nif]
pub fn sum_list(iter: ListIterator) -> NifResult<i64> {
    // Do nothing and suppress panic message. From https://stackoverflow.com/a/35559417
    panic::set_hook(Box::new(|_info| {}));

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
pub fn sum_list_as_floats(list: Vec<f64>) -> f64 {
    list.into_iter().sum()
}
