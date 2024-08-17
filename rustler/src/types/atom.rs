use crate::wrapper::atom;
use crate::wrapper::NIF_TERM;
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};

// Atoms are a special case of a term. They can be stored and used on all envs regardless of where
// it lives and when it is created.
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Atom {
    term: NIF_TERM,
}

impl Atom {
    pub fn as_c_arg(self) -> NIF_TERM {
        self.term
    }

    pub fn to_term(self, env: Env) -> Term {
        // Safe because atoms are not associated with any environment.
        unsafe { Term::new(env, self.term) }
    }

    unsafe fn from_nif_term(term: NIF_TERM) -> Self {
        Atom { term }
    }

    pub fn from_term(term: Term) -> NifResult<Self> {
        if term.is_atom() {
            Ok(unsafe { Atom::from_nif_term(term.as_c_arg()) })
        } else {
            Err(Error::BadArg)
        }
    }

    /// Return the atom whose text representation is `bytes`, like `erlang:binary_to_atom/2`.
    ///
    /// # Errors
    /// `Error::BadArg` if `bytes.len() > 255`.
    pub fn from_bytes(env: Env, bytes: &[u8]) -> NifResult<Atom> {
        if bytes.len() > 255 {
            return Err(Error::BadArg);
        }
        unsafe { Ok(Atom::from_nif_term(atom::make_atom(env.as_c_arg(), bytes))) }
    }

    /// Return the atom whose text representation is `bytes`, like `erlang:binary_to_existing_atom/2`, if atom with given text representation exists.
    ///
    /// # Errors
    /// `Error::BadArg` if `bytes.len() > 255`.
    pub fn try_from_bytes(env: Env, bytes: &[u8]) -> NifResult<Option<Atom>> {
        if bytes.len() > 255 {
            return Err(Error::BadArg);
        }
        unsafe {
            match atom::make_existing_atom(env.as_c_arg(), bytes) {
                Some(term) => Ok(Some(Atom::from_nif_term(term))),
                None => Ok(None),
            }
        }
    }

    /// Return the atom whose text representation is the given `string`, like `erlang:list_to_atom/2`.
    ///
    /// # Errors
    /// `Error::BadArg` if `string` contains characters that aren't in Latin-1, or if it's too
    /// long. The maximum length is 255 characters.
    pub fn from_str(env: Env, string: &str) -> NifResult<Atom> {
        if string.is_ascii() {
            // Fast path.
            Atom::from_bytes(env, string.as_bytes())
        } else {
            // Convert from Rust UTF-8 to Latin-1.
            let mut bytes = Vec::with_capacity(string.len());
            for c in string.chars() {
                if (c as u32) >= 256 {
                    return Err(Error::BadArg);
                }
                bytes.push(c as u8);
            }
            Atom::from_bytes(env, &bytes)
        }
    }
}

use std::fmt;
impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        crate::wrapper::term::fmt(self.as_c_arg(), f)
    }
}

impl Encoder for Atom {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.to_term(env)
    }
}
impl<'a> Decoder<'a> for Atom {
    fn decode(term: Term<'a>) -> NifResult<Atom> {
        Atom::from_term(term)
    }
}

impl<'a> PartialEq<Term<'a>> for Atom {
    fn eq(&self, other: &Term<'a>) -> bool {
        self.as_c_arg() == other.as_c_arg()
    }
}

/// ## Atom terms
impl<'a> Term<'a> {
    /// When the term is an atom, this method will return the string
    /// representation of it.
    ///
    /// If you only need to test for equality, comparing the terms directly
    /// is much faster.
    ///
    /// Will return None if the term is not an atom.
    pub fn atom_to_string(&self) -> NifResult<String> {
        unsafe { atom::get_atom(self.get_env().as_c_arg(), self.as_c_arg()) }
    }
}

pub fn is_truthy(term: Term) -> bool {
    !((term.as_c_arg() == false_().as_c_arg()) || (term.as_c_arg() == nil().as_c_arg()))
}

pub(in crate::types) fn decode_bool(term: Term) -> NifResult<bool> {
    let as_c_arg = term.as_c_arg();

    if as_c_arg == true_().as_c_arg() {
        return Ok(true);
    }

    if as_c_arg == false_().as_c_arg() {
        return Ok(false);
    }

    Err(Error::BadArg)
}

// This is safe because atoms are never removed/changed once they are created.
unsafe impl Sync for Atom {}
unsafe impl Send for Atom {}

/// Macro for defining Rust functions that return Erlang atoms.
///
/// For example, this code:
///
///     mod my_atoms {
///         rustler::atoms! {
///             jpeg,
///         }
///     }
///     # fn main() {}
///
/// defines a public function `my_atoms::jpeg()` that returns the `Atom` for the `jpeg` atom.
///
/// Multiple atoms can be defined. Each one can have its own doc comment and other attributes.
///
///     rustler::atoms! {
///         /// The `jpeg` atom.
///         jpeg,
///
///         /// The `png` atom.
///         png,
///
///         #[allow(non_snake_case)]
///         WebP,
///     }
///     # fn main() {}
///
/// When you need an atom that's not a legal Rust function name, write `NAME = "ATOM"`, like
/// this:
///
///     rustler::atoms! {
///         /// The `mod` atom. The function isn't called `mod` because that's
///         /// a Rust keyword.
///         mod_atom = "mod",
///
///         /// The atom `'hello world'`. Obviously this function can't be
///         /// called `hello world` because there's a space in it.
///         hello_world = "hello world",
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
macro_rules! atoms {
    {
        $(
            $( #[$attr:meta] )*
            $name:ident $( = $str:expr )?
        ),*$(,)?
    } => {
        #[allow(non_snake_case)]
        struct RustlerAtoms {
            $( $name : $crate::types::atom::Atom ),*
        }
        impl RustlerAtoms {
            fn get() -> &'static Self {
                use std::sync::OnceLock;
                static RUSTLER_ATOMS: OnceLock<RustlerAtoms> = OnceLock::new();
                RUSTLER_ATOMS.get_or_init(||
                    $crate::env::OwnedEnv::new().run(|env| {
                        RustlerAtoms {
                            $( $name: $crate::atoms!(@internal_make_atom(env, $name $( = $str)? )) ),*
                        }
                    })
                )
            }
        }
        $(
            $( #[$attr] )*
            pub fn $name() -> $crate::types::atom::Atom {
                RustlerAtoms::get().$name
            }
        )*
    };

    // Internal helper macros.
    { @internal_make_atom($env:ident, $name:ident) } => {
        $crate::atoms!(@internal_make_atom($env, $name = stringify!($name)))
    };
    { @internal_make_atom($env:ident, $name:ident = $str:expr) } => {
        $crate::types::atom::Atom::from_str($env, $str)
            .expect("rustler::atoms!: bad atom string")
    };
}

atoms! {
    /// The `nif_panicked` atom.
    nif_panicked,

    /// The `nil` atom.
    nil,

    /// The `undefined` atom, commonly used in Erlang libraries to express the
    /// absence of value.
    undefined,

    /// The `ok` atom, commonly used in success tuples.
    ok,

    /// The `error` atom, commonly used in error tuples.
    error,

    /// The `badarg` atom, which Rustler sometimes returns to indicate that a function was
    /// called with incorrect arguments.
    badarg,

    /// The `false` atom. (Trailing underscore because `false` is a keyword in Rust.)
    ///
    /// If you're looking to convert between Erlang terms and Rust `bool`
    /// values, use `Encoder` and `Decoder` instead.
    false_ = "false",

    /// The `true` atom. (Trailing underscore because `true` is a keyword in Rust.)
    ///
    /// If you're looking to convert between Erlang terms and Rust `bool`
    /// values, use `Encoder` and `Decoder` instead.
    true_ = "true",

    /// The `__struct__` atom used by Elixir.
    __struct__,

    /// The `first` atom used by `Elixir.Range`.
    first,

    /// The `last` atom used by `Elixir.Range`.
    last,

    /// The `step` atom used by `Elixir.Range` vor Elixir >= v1.12
    step,
}
