use rustler::{NifEnv, NifTerm, NifEncoder, NifResult};
use rustler::types::binary::NifBinary;

pub fn make_shorter_subbinary<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let binary: NifBinary = try!(args[0].decode());
    let length: usize = binary.as_slice().len();
    Ok(try!(binary.make_subbinary(1, length-2)).encode(env))
}
