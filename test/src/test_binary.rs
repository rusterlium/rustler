use rustler::{NifEnv, NifTerm, NifEncoder, NifResult, NifError};
use rustler::types::binary::NifBinary;

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
