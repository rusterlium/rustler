use std::io::Write;
use std::panic;

use rustler::types::binary::{Binary, NewBinary, OwnedBinary};
use rustler::{Env, Error, NifResult, Term};

#[rustler::nif]
pub fn make_shorter_subbinary(binary: Binary) -> NifResult<Binary> {
    let length: usize = binary.as_slice().len();
    binary.make_subbinary(1, length - 2)
}

#[rustler::nif]
pub fn parse_integer(string: &str) -> NifResult<i64> {
    std::str::FromStr::from_str(string).map_err(|_| Error::BadArg)
}

#[rustler::nif]
pub fn binary_new(env: Env) -> Binary {
    let mut binary = OwnedBinary::new(4).unwrap();
    binary.as_mut_slice().write_all(&[1, 2, 3, 4]).unwrap();
    binary.release(env)
}

#[rustler::nif]
pub fn owned_binary_new() -> OwnedBinary {
    let mut binary = OwnedBinary::new(4).unwrap();
    binary.as_mut_slice().write_all(&[1, 2, 3, 4]).unwrap();
    binary
}

#[rustler::nif]
pub fn new_binary_new(env: Env) -> Binary {
    let mut binary = NewBinary::new(env, 4);
    binary.as_mut_slice().write_all(&[1, 2, 3, 4]).unwrap();
    binary.into()
}

#[rustler::nif]
pub fn unowned_to_owned<'a>(env: Env<'a>, binary: Binary<'a>) -> NifResult<Binary<'a>> {
    // Do nothing and suppress panic message. From https://stackoverflow.com/a/35559417
    panic::set_hook(Box::new(|_info| {}));

    let mut copied = binary.to_owned().unwrap();
    copied.as_mut_slice()[0] = 1;
    Ok(copied.release(env))
}

#[rustler::nif]
pub fn realloc_shrink(env: Env) -> Binary {
    let mut binary = OwnedBinary::new(8).unwrap();
    binary
        .as_mut_slice()
        .write_all(&[1, 2, 3, 4, 5, 6, 7, 8])
        .unwrap();
    if !binary.realloc(4) {
        panic!("Realloc failed");
    }
    binary.release(env)
}

#[rustler::nif]
pub fn realloc_grow(env: Env) -> Binary {
    let mut binary = OwnedBinary::new(4).unwrap();
    binary.as_mut_slice().write_all(&[1, 2, 3, 4]).unwrap();
    binary.realloc_or_copy(5);
    binary.as_mut_slice()[4] = 5;
    binary.release(env)
}

#[rustler::nif]
pub fn encode_string() -> (String, &'static str) {
    ("first".to_string(), "second")
}

#[rustler::nif]
pub fn decode_iolist(binary: Term) -> NifResult<Binary> {
    binary.decode_as_binary()
}

#[rustler::nif]
pub fn first_four_bytes_of_iolist<'a>(term: Term<'a>) -> Binary<'a> {
    let bin = Binary::from_iolist(term).unwrap();
    let sub = bin.make_subbinary(0, 4).unwrap();
    sub
}

#[rustler::nif]
pub fn subbinary_as_term<'a>(binary: Binary<'a>, offset: usize, length: usize) -> NifResult<Term<'a>> {
    binary.make_subbinary_term(offset, length)
}
