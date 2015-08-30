use std::collections::HashMap;
use std::sync::Mutex;
use std::cell::RefCell;
use super::{ ERL_NIF_TERM };

// Atoms are a special case of a term. They can be stored and used on all envs regardless of where
// it lives and when it is created.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct NifAtom {
    term: ERL_NIF_TERM,
}

// This should be safe to do because atoms are never removed/changed once they are created.
unsafe impl Sync for NifAtom {}
unsafe impl Send for NifAtom {}

lazy_static! {
    static ref ATOMS: Mutex<RefCell<HashMap<&'static str, NifAtom>>> = Mutex::new(RefCell::new(HashMap::new()));
}

