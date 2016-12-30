use std::collections::HashMap;
use std::sync::RwLock;
use std::sync::Mutex;
use std::ops::DerefMut;

use super::{ NifTerm, NifEnv };
use wrapper::nif_interface::{
    ErlNifCharEncoding,
    NIF_ENV,
    NIF_TERM,
    c_uint,
    enif_alloc_env,
    enif_get_atom,
    enif_get_atom_length,
    enif_make_atom_len,
    size_t,
};

// Atoms are a special case of a term. They can be stored and used on all envs regardless of where
// it lives and when it is created.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct NifAtom {
    term: NIF_TERM,
}
impl NifAtom {
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
    pub fn from_term(env: &NifEnv, term: NifTerm) -> Option<Self> {
        match is_atom(env, term) {
            true => Some(unsafe { NifAtom::from_nif_term(term.as_c_arg()) }),
            false => None
        }
    }

    /// Get the contents of this atom as a string.
    ///
    /// (This is slow, and for most cases, unnecessary. For example, if you
    /// need to know whether or not this atom is `true`, try comparing it to
    /// `get_atom_init("true")`.)
    ///
    pub fn to_string(&self, env: &NifEnv) -> String {
        // Determine the length of the atom, in bytes.
        let mut len = 0;
        let success = unsafe {
            enif_get_atom_length(env.as_c_arg(), self.term, &mut len,
                                 ErlNifCharEncoding::ERL_NIF_LATIN1)
        };
        assert_eq!(success, 1);

        // Get the bytes from the atom into a buffer.
        // enif_get_atom() writes a null terminated string,
        // so add 1 to the atom's length to make room for it.
        let mut bytes: Vec<u8> = Vec::with_capacity(len as usize + 1);
        let nbytes = unsafe {
            enif_get_atom(env.as_c_arg(), self.term, bytes.as_mut_ptr(), len + 1,
                          ErlNifCharEncoding::ERL_NIF_LATIN1)
        };
        assert!(nbytes as c_uint == len + 1);
        unsafe {
            bytes.set_len(len as usize);  // ignore the null byte
        }

        // Convert the Latin-1 bytes to String.
        bytes.into_iter().map(|b| b as char).collect()
    }
}

pub fn is_atom(env: &NifEnv, term: NifTerm) -> bool {
    ::wrapper::check::is_atom(env.as_c_arg(), term.as_c_arg())
}

pub fn is_truthy(term: NifTerm) -> bool {
    !((term.term == get_atom("false").unwrap().term) || (term.term == get_atom("nil").unwrap().term))
}

// This should be safe to do because atoms are never removed/changed once they are created.
unsafe impl Sync for NifAtom {}
unsafe impl Send for NifAtom {}

struct PrivNifEnvWrapper {
    env: NIF_ENV
}
unsafe impl Send for PrivNifEnvWrapper {}

lazy_static! {
    //static ref ATOMS: Mutex<RefCell<HashMap<&'static str, NifAtom>>> = Mutex::new(RefCell::new(HashMap::new()));
    static ref ATOMS: RwLock<HashMap<&'static str, NifAtom>> = {
        let mut map = HashMap::new();

        let mut env_guard = ATOM_ENV.lock().unwrap();
        let env = env_guard.deref_mut();

        map.insert("true", unsafe { NifAtom::make_atom(env.env, "true") });
        map.insert("false", unsafe { NifAtom::make_atom(env.env, "false") });
        map.insert("nil", unsafe { NifAtom::make_atom(env.env, "nil") });

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
