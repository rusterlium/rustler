use std::collections::HashMap;
use std::sync::RwLock;
use std::sync::Mutex;
use std::ops::DerefMut;

extern crate ruster_unsafe;
use super::{ ERL_NIF_TERM, size_t, NifTerm, NifEnv };

// Atoms are a special case of a term. They can be stored and used on all envs regardless of where
// it lives and when it is created.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct NifAtom {
    term: ERL_NIF_TERM,
}
impl NifAtom {
    unsafe fn from_nif_term(term: ERL_NIF_TERM) -> Self {
        NifAtom {
            term: term
        }
    }
    pub fn to_term<'a>(self, env: &'a NifEnv) -> NifTerm<'a> {
        NifTerm::new(env, self.term)
    }
    unsafe fn make_atom(env: *mut ruster_unsafe::ErlNifEnv, name: &str) -> Self {
        NifAtom::from_nif_term(ruster_unsafe::enif_make_atom_len(env, name.as_ptr() as *const u8, name.len() as size_t))
    }
}

pub fn is_term_truthy(term: NifTerm, env: &NifEnv) -> bool {
    !((term.term == get_atom("false").unwrap().term) || (term.term == get_atom("nil").unwrap().term))
}

// This should be safe to do because atoms are never removed/changed once they are created.
unsafe impl Sync for NifAtom {}
unsafe impl Send for NifAtom {}

struct PrivNifEnvWrapper {
    env: *mut ruster_unsafe::ErlNifEnv
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
        Mutex::new(PrivNifEnvWrapper { env: unsafe { ruster_unsafe::enif_alloc_env() } })
    };
}

pub fn init_atom(name: &'static str) {
    let mut atoms = ATOMS.write().unwrap();
    let mut env_guard = ATOM_ENV.lock().unwrap();
    let env = env_guard.deref_mut();
    atoms.insert(name, unsafe { NifAtom::make_atom(env.env, name) });
}

pub fn get_atom(name: &str) -> Option<NifAtom> {
    ATOMS.read().unwrap().get(name).cloned()
}
pub fn get_atom_init(name: &'static str) -> NifAtom {
    let mut atoms = ATOMS.write().unwrap();
    if atoms.contains_key(name) {
        atoms.get(name).unwrap().clone()
    } else {
        let mut env_guard = ATOM_ENV.lock().unwrap();
        let env = env_guard.deref_mut();
        let atom = unsafe { NifAtom::make_atom(env.env, name) };
        atoms.insert(name, atom);
        atom
    }
}
