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
    Atom::from_bytes(env, binary.as_slice())
}

#[rustler::nif]
pub fn binary_to_atom_utf8(env: Env, binary: Binary) -> NifResult<Atom> {
    Atom::from_utf8_bytes(env, binary.as_slice())
}

#[rustler::nif]
pub fn binary_to_existing_atom(env: Env, binary: Binary) -> Option<Atom> {
    Atom::try_from_bytes(env, binary.as_slice()).ok()
}

#[rustler::nif]
pub fn binary_to_existing_atom_utf8(env: Env, binary: Binary) -> Option<Atom> {
    Atom::try_from_utf8_bytes(env, binary.as_slice()).ok()
}
