use rustler;
use rustler::{NifEnv, NifTerm, NifEncoder, NifResult};

#[derive(NifTuple)]
struct AddTuple {
    lhs: i32,
    rhs: i32,
}

pub fn tuple_echo<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let tuple: AddTuple = try!(args[0].decode());
    Ok(tuple.encode(env))
}

#[derive(NifMap)]
struct AddMap {
    lhs: i32,
    rhs: i32,
}

pub fn map_echo<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let map: AddMap = try!(args[0].decode());
    Ok(map.encode(env))
}
