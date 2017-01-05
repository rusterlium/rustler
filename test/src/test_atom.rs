use rustler::NifEncoder;
use rustler::{NifEnv, NifTerm, NifResult};

pub fn on_load(_env: NifEnv) {
}

pub fn atom_to_string<'a>(env: NifEnv<'a>, args: &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>> {
    let atom_string = try!(args[0].atom_to_string());
    Ok(atom_string.encode(env))
}
