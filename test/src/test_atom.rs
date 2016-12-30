use rustler::{NifEnv, NifTerm, NifError, NifEncoder, NifResult};
use rustler::atom::NifAtom;

pub fn atom_to_string<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let atom: NifAtom = match NifAtom::from_term(env, args[0]) {
        Some(atom) => atom,
        None => return Err(NifError::BadArg)
    };
    let string = atom.to_string(env);
    Ok(string.encode(env))
}
