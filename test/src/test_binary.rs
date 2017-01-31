use ::std::io::Write;

use rustler::{NifEnv, NifTerm, NifEncoder, NifResult, NifError};
use rustler::types::binary::{ NifBinary, OwnedNifBinary };

pub fn make_shorter_subbinary<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let binary: NifBinary = try!(args[0].decode());
    let length: usize = binary.as_slice().len();
    Ok(try!(binary.make_subbinary(1, length-2)).encode(env))
}

pub fn parse_integer<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let str_num: &str = args[0].decode()?;
    let num: i64 = match ::std::str::FromStr::from_str(str_num) {
        Ok(num) => num,
        Err(_) => return Err(NifError::BadArg),
    };
    Ok(num.encode(env))
}

pub fn binary_new<'a>(env: NifEnv<'a>, _args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let mut binary = OwnedNifBinary::new(4).unwrap();
    binary.as_mut_slice().write(&[1, 2, 3, 4]).unwrap();
    Ok(binary.release(env).encode(env))
}

pub fn unowned_to_owned<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let in_binary: NifBinary = args[0].decode()?;
    let mut copied = in_binary.to_owned().unwrap();
    copied.as_mut_slice()[0] = 1;
    Ok(copied.release(env).encode(env))
}

pub fn realloc_shrink<'a>(env: NifEnv<'a>, _args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let mut binary = OwnedNifBinary::new(8).unwrap();
    binary.as_mut_slice().write(&[1, 2, 3, 4, 5, 6, 7, 8]).unwrap();
    if !binary.realloc(4) {
        panic!("Realloc failed");
    }
    Ok(binary.release(env).encode(env))
}

pub fn realloc_grow<'a>(env: NifEnv<'a>, _args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let mut binary = OwnedNifBinary::new(4).unwrap();
    binary.as_mut_slice().write(&[1, 2, 3, 4]).unwrap();
    binary.realloc_or_copy(5);
    binary.as_mut_slice()[4] = 5;
    Ok(binary.release(env).encode(env))
}

// OwnedNifBinary::new
// OwnedNifBinary::from_unowned
// OwnedNifBinary::realloc
// OwnedNifBinary::realloc_or_copy
