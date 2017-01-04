use std::collections::HashMap;
use std::sync::RwLock;
use std::sync::Mutex;
use std::ops::DerefMut;

use ::{ NifTerm, NifEnv, NifResult, NifError };
use ::wrapper::nif_interface::{
    NIF_ENV,
    NIF_TERM,
    enif_make_atom_len,
    enif_alloc_env,
    size_t,
};

// Atoms are a special case of a term. They can be stored and used on all envs regardless of where
// it lives and when it is created.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct NifAtom {
    term: NIF_TERM,
}

impl NifAtom {
    pub fn as_c_arg(&self) -> NIF_TERM {
        self.term
    }

    pub fn to_term<'a>(self, env: &'a NifEnv) -> NifTerm<'a> {
        NifTerm::new(env, self.term)
    }
    unsafe fn make_atom(env: NIF_ENV, name: &str) -> Self {
        NifAtom::from_nif_term(enif_make_atom_len(env, name.as_ptr() as *const u8, name.len() as size_t))
    }

    unsafe fn from_nif_term(term: NIF_TERM) -> Self {
        NifAtom {
            term: term
        }
    }
    pub fn from_term(term: NifTerm) -> NifResult<Self> {
        match term.is_atom() {
            true => Ok(unsafe { NifAtom::from_nif_term(term.as_c_arg()) }),
            false => Err(NifError::BadArg)
        }
    }
}

/// ## Atom terms
impl<'a> NifTerm<'a> {

    /// When the term is an atom, this method will return the string
    /// representation of it.
    ///
    /// If you only need to test for equality, comparing the terms directly
    /// is much faster.
    ///
    /// Will return None if the term is not an atom.
    pub fn atom_to_string(&self) -> NifResult<String> {
        ::wrapper::atom::get_atom(self.get_env().as_c_arg(), self.as_c_arg())
    }

}

pub fn is_atom(env: &NifEnv, term: NifTerm) -> bool {
    ::wrapper::check::is_atom(env.as_c_arg(), term.as_c_arg())
}

pub fn is_truthy(term: NifTerm) -> bool {
    !((term.as_c_arg() == get_atom("false").unwrap().as_c_arg()) || (term.as_c_arg() == get_atom("nil").unwrap().as_c_arg()))
}

// This should be safe to do because atoms are never removed/changed once they are created.
unsafe impl Sync for NifAtom {}
unsafe impl Send for NifAtom {}

struct PrivNifEnvWrapper {
    env: NIF_ENV
}
unsafe impl Send for PrivNifEnvWrapper {}

lazy_static! {
    static ref ATOMS: RwLock<HashMap<&'static str, NifAtom>> = {
        let mut map = HashMap::new();

        let mut env_guard = ATOM_ENV.lock().unwrap();
        let env = env_guard.deref_mut();

        map.insert("true", unsafe { NifAtom::make_atom(env.env, "true") });
        map.insert("false", unsafe { NifAtom::make_atom(env.env, "false") });
        map.insert("nil", unsafe { NifAtom::make_atom(env.env, "nil") });
        map.insert("__struct__", unsafe { NifAtom::make_atom(env.env, "__struct__") });

        RwLock::new(map)
    };

    static ref ATOM_ENV: Mutex<PrivNifEnvWrapper> = {
        Mutex::new(PrivNifEnvWrapper { env: unsafe { enif_alloc_env() } })
    };
}

pub fn init_atom(name: &'static str) -> NifAtom {
    let mut atoms = ATOMS.write().unwrap();
    let mut env_guard = ATOM_ENV.lock().unwrap();
    let env = env_guard.deref_mut();
    let atom = unsafe { NifAtom::make_atom(env.env, name) };
    atoms.insert(name, atom);
    atom
}

pub fn get_atom(name: &str) -> Option<NifAtom> {
    ATOMS.read().unwrap().get(name).cloned()
}

pub fn get_atom_init(name: &'static str) -> NifAtom {
    match get_atom(name) {
        Some(atom) => return atom,
        _ => (),
    }
    init_atom(name)
}
