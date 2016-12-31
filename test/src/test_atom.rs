use rustler::NifEncoder;
use rustler::{NifEnv, NifTerm, NifResult};

pub fn on_load(env: &NifEnv) {
}

pub fn atom_to_string<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let atom_string = args[0].atom_to_string().unwrap();
    Ok(atom_string.encode(env))
}
