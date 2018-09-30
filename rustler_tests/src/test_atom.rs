use rustler::types::{atom::Atom, binary::Binary};
use rustler::Encoder;
use rustler::{Env, NifResult, Term};

mod atoms {
    rustler_atoms! {
        atom ok;
    }
}

pub fn on_load(_env: Env) {}

pub fn atom_to_string<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let atom_string = try!(args[0].atom_to_string());
    Ok(atom_string.encode(env))
}

pub fn atom_equals_ok<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    Ok((atoms::ok() == args[0]).encode(env))
}

pub fn binary_to_atom<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let binary: Binary = args[0].decode()?;
    let atom = Atom::from_bytes(env, binary.as_slice())?;
    Ok(atom.encode(env))
}

pub fn binary_to_existing_atom<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let binary: Binary = args[0].decode()?;
    let atom = Atom::try_from_bytes(env, binary.as_slice())?;
    Ok(atom.encode(env))
}
