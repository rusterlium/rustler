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

    pub fn to_term<'a>(self, env: NifEnv<'a>) -> NifTerm<'a> {
        NifTerm::new(env, self.term)
    }

    #[doc(hidden)]
    pub unsafe fn make_atom(env: NIF_ENV, name: &str) -> Self {
        NifAtom::from_nif_term(enif_make_atom_len(env, name.as_ptr() as *const u8, name.len()))
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
        unsafe { ::wrapper::atom::get_atom(self.get_env().as_c_arg(), self.as_c_arg()) }
    }

}

pub fn is_truthy(term: NifTerm) -> bool {
    !((term.as_c_arg() == false_().as_c_arg()) || (term.as_c_arg() == nil().as_c_arg()))
}

// This should be safe to do because atoms are never removed/changed once they are created.
unsafe impl Sync for NifAtom {}
unsafe impl Send for NifAtom {}

struct PrivNifEnvWrapper {
    env: NIF_ENV
}
unsafe impl Send for PrivNifEnvWrapper {}

lazy_static! {
    static ref ATOMS: RwLock<HashMap<&'static str, NifAtom>> = RwLock::new(HashMap::new());

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


/// Macro for defining Rust functions that return Erlang atoms.
/// To use this macro, you must also import the `lazy_static` crate.
///
/// For example, this code:
///
///     #[macro_use] extern crate rustler;
///     #[macro_use] extern crate lazy_static;
///
///     mod my_atoms {
///         rustler_atoms! {
///             atom jpeg;
///         }
///     }
///     # fn main() {}
///
/// defines a public function `my_atoms::jpeg()` that returns the `NifAtom` for the `jpeg` atom.
///
/// Multiple atoms can be defined. Each one can have its own doc comment and other attributes.
///
///     # #[macro_use] extern crate rustler;
///     # #[macro_use] extern crate lazy_static;
///     rustler_atoms! {
///         /// The `jpeg` atom.
///         atom jpeg;
///
///         /// The `png` atom.
///         atom png;
///
///         #[allow(non_snake_case)]
///         atom WebP;
///     }
///     # fn main() {}
///
/// When you need an atom that's not a legal Rust function name, write `atom NAME = "ATOM"`, like
/// this:
///
///     # #[macro_use] extern crate rustler;
///     # #[macro_use] extern crate lazy_static;
///     rustler_atoms! {
///         /// The `mod` atom. The function isn't called `mod` because that's
///         /// a Rust keyword.
///         atom mod_atom = "mod";
///
///         /// The atom `'hello world'`. Obviously this function can't be
///         /// called `hello world` because there's a space in it.
///         atom hello_world = "hello world";
///     }
///     # fn main() {}
///
/// # Performance
///
/// These functions are faster than `get_atom` and `get_atom_init`. The first time you call one, it
/// creates atoms for all its sibling functions and caches them, so that all later calls are fast.
/// The only overhead is checking that the atoms have been created (an atomic integer read).
///
#[macro_export]
macro_rules! rustler_atoms {
    {
        $(
            $( #[$attr:meta] )*
            atom $name:ident $( = $str:expr )*;
        )*
    } => {
        #[allow(non_snake_case)]
        struct RustlerAtoms {
            $( $name : $crate::types::atom::NifAtom ),*
        }
        lazy_static! {
            static ref RUSTLER_ATOMS: RustlerAtoms = $crate::env::OwnedEnv::new().run(|env| {
                RustlerAtoms {
                    $( $name: rustler_atoms!(@internal_make_atom(env, $name $( = $str)* )) ),*
                }
            });
        }
        $(
            $( #[$attr] )*
            pub fn $name() -> $crate::types::atom::NifAtom {
                RUSTLER_ATOMS.$name
            }
        )*
    };

    // Internal helper macros.
    { @internal_make_atom($env:ident, $name:ident) } => {
        rustler_atoms!(@internal_make_atom($env, $name = stringify!($name)))
    };
    { @internal_make_atom($env:ident, $name:ident = $str:expr) } => {
        unsafe { $crate::types::atom::NifAtom::make_atom($env.as_c_arg(), $str) }
    };
}

rustler_atoms! {
    /// The `nil` atom.
    atom nil;

    /// The `ok` atom, commonly used in success tuples.
    atom ok;

    /// The `error` atom, commonly used in error tuples.
    atom error;

    /// The `badarg` atom, which Rustler sometimes returns to indicate that a function was
    /// called with incorrect arguments.
    atom badarg;

    /// The `false` atom. (Trailing underscore because `false` is a keyword in Rust.)
    ///
    /// If you're looking to convert between Erlang terms and Rust `bool`
    /// values, use `NifEncoder` and `NifDecoder` instead.
    atom false_ = "false";

    /// The `true` atom. (Trailing underscore because `true` is a keyword in Rust.)
    ///
    /// If you're looking to convert between Erlang terms and Rust `bool`
    /// values, use `NifEncoder` and `NifDecoder` instead.
    atom true_ = "true";

    /// The `__struct__` atom used by Elixir.
    atom __struct__;
}
