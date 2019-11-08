use rustler::{Atom, Binary, Env, NifResult, Term};

mod atoms {
    rustler::atoms! { ok }
}

#[rustler::nif]
pub fn atom_to_string(atom: Term) -> NifResult<String> {
    atom.atom_to_string()
}

#[rustler::nif]
pub fn atom_equals_ok(atom: Atom) -> bool {
    atoms::ok() == atom
}

#[rustler::nif]
pub fn binary_to_atom(env: Env, binary: Binary) -> NifResult<Atom> {
    let atom = Atom::from_bytes(env, binary.as_slice())?;
    Ok(atom)
}

#[rustler::nif]
pub fn binary_to_existing_atom(env: Env, binary: Binary) -> NifResult<Option<Atom>> {
    let atom = Atom::try_from_bytes(env, binary.as_slice())?;
    Ok(atom)
}
