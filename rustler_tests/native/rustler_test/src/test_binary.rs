use ::std::io::Write;

use rustler::{Env, Term, NifResult, Error};
use rustler::types::binary::{ Binary, OwnedBinary };

pub fn make_shorter_subbinary<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<Binary<'a>> {
    let binary: Binary = args[0].decode()?;
    let length: usize = binary.as_slice().len();
    Ok(binary.make_subbinary(1, length-2)?)
}

pub fn parse_integer<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<i64> {
    let str_num: &str = args[0].decode()?;
    let num: i64 = match ::std::str::FromStr::from_str(str_num) {
        Ok(num) => num,
        Err(_) => return Err(Error::BadArg),
    };
    Ok(num)
}

pub fn binary_new<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Binary<'a>> {
    let mut binary = OwnedBinary::new(4).unwrap();
    binary.as_mut_slice().write(&[1, 2, 3, 4]).unwrap();
    Ok(binary.release(env))
}

pub fn unowned_to_owned<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Binary<'a>> {
    let in_binary: Binary = args[0].decode()?;
    let mut copied = in_binary.to_owned().unwrap();
    copied.as_mut_slice()[0] = 1;
    Ok(copied.release(env))
}

pub fn realloc_shrink<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Binary<'a>> {
    let mut binary = OwnedBinary::new(8).unwrap();
    binary.as_mut_slice().write(&[1, 2, 3, 4, 5, 6, 7, 8]).unwrap();
    if !binary.realloc(4) {
        panic!("Realloc failed");
    }
    Ok(binary.release(env))
}

pub fn realloc_grow<'a>(env: Env<'a>, _args: &[Term<'a>]) -> NifResult<Binary<'a>> {
    let mut binary = OwnedBinary::new(4).unwrap();
    binary.as_mut_slice().write(&[1, 2, 3, 4]).unwrap();
    binary.realloc_or_copy(5);
    binary.as_mut_slice()[4] = 5;
    Ok(binary.release(env))
}

pub fn encode_string<'a>(_env: Env<'a>, _args: &[Term<'a>]) -> NifResult<(String, &'static str)> {
    Ok(("first".to_string(), "second"))
}

// OwnedBinary::new
// OwnedBinary::from_unowned
// OwnedBinary::realloc
// OwnedBinary::realloc_or_copy
