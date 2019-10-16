use rustler::types::{atom::Atom, binary::Binary};
use rustler::{Env, NifResult, Term};

mod atoms {
    rustler::atoms! {
        ok;
    }
}

pub fn on_load(_env: Env) {}

pub fn atom_to_string<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<String> {
    args[0].atom_to_string()
}

pub fn atom_equals_ok<'a>(_env: Env<'a>, args: &[Term<'a>]) -> bool {
    atoms::ok() == args[0]
}

pub fn binary_to_atom<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Atom> {
    let binary: Binary = args[0].decode()?;
    let atom = Atom::from_bytes(env, binary.as_slice())?;
    Ok(atom)
}

pub fn binary_to_existing_atom<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Option<Atom>> {
    let binary: Binary = args[0].decode()?;
    let atom = Atom::try_from_bytes(env, binary.as_slice())?;
    Ok(atom)
}
