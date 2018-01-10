use rustler::Encoder;
use rustler::{NifEnv, NifTerm, NifResult};

mod atoms {
    rustler_atoms! {
        atom ok;
    }
}

pub fn on_load(_env: NifEnv) {
}

pub fn atom_to_string<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let atom_string = try!(args[0].atom_to_string());
    Ok(atom_string.encode(env))
}

pub fn atom_equals_ok<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    Ok((atoms::ok() == args[0]).encode(env))
}
