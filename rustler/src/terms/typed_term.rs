//! Runtime classification into typed raw term wrappers.

#[cfg(feature = "nif_version_2_15")]
use super::Bitstring;
use super::{Atom, Binary, Float, Fun, Integer, List, LocalPid, Map, Pid, Port, Reference, Tuple};
use crate::sys::ErlNifTerm;
use crate::{BoundTerm, Decoder, Encoder, Env, Error, NifResult, Term, TermType};

/// A supported Erlang term classified into its typed wrapper.
///
/// Local pids select `LocalPid`; remote pids select `Pid`. Byte-aligned bitstrings select
/// `Binary`; other bitstrings select `Bitstring` when NIF 2.15 is enabled. Unsupported types
/// return `Error::BadArg` during decoding.
///
/// The enum as a whole is bound to an environment, even though individual `Atom` and
/// `LocalPid` payloads can be extracted and used independently.
///
/// ```compile_fail
/// use rustler::{Term, TypedTerm};
///
/// fn escape<'a>(term: Term<'a>) -> TypedTerm<'static> {
///     term.decode().unwrap()
/// }
/// ```
#[derive(Clone, Copy)]
pub enum TypedTerm<'a> {
    Atom(Atom),
    Binary(Binary<'a>),
    #[cfg(feature = "nif_version_2_15")]
    Bitstring(Bitstring<'a>),
    Float(Float<'a>),
    Fun(Fun<'a>),
    Integer(Integer<'a>),
    List(List<'a>),
    LocalPid(LocalPid),
    Map(Map<'a>),
    Pid(Pid<'a>),
    Port(Port<'a>),
    Reference(Reference<'a>),
    Tuple(Tuple<'a>),
}

impl<'a> TypedTerm<'a> {
    /// Classify a term without copying it.
    pub fn from_term(term: Term<'a>) -> NifResult<Self> {
        match term.get_type() {
            TermType::Atom => Atom::from_term(term).map(Self::Atom),
            TermType::Binary => {
                if term.is_binary() {
                    Binary::from_term(term).map(Self::Binary)
                } else {
                    #[cfg(feature = "nif_version_2_15")]
                    return Bitstring::from_term(term).map(Self::Bitstring);
                    #[cfg(not(feature = "nif_version_2_15"))]
                    return Err(Error::BadArg);
                }
            }
            TermType::Float => Float::from_term(term).map(Self::Float),
            TermType::Fun => Fun::from_term(term).map(Self::Fun),
            TermType::Integer => Integer::from_term(term).map(Self::Integer),
            TermType::List => List::from_term(term).map(Self::List),
            TermType::Map => Map::from_term(term).map(Self::Map),
            TermType::Pid => match LocalPid::from_term(term) {
                Ok(pid) => Ok(Self::LocalPid(pid)),
                Err(Error::BadArg) => Pid::from_term(term).map(Self::Pid),
                Err(error) => Err(error),
            },
            TermType::Port => Port::from_term(term).map(Self::Port),
            TermType::Ref => Reference::from_term(term).map(Self::Reference),
            TermType::Tuple => Tuple::from_term(term).map(Self::Tuple),
            TermType::Unknown => Err(Error::BadArg),
        }
    }

    /// Return the raw term held by the selected variant.
    pub fn as_c_arg(&self) -> ErlNifTerm {
        match self {
            Self::Atom(term) => term.as_c_arg(),
            Self::Binary(term) => term.as_c_arg(),
            #[cfg(feature = "nif_version_2_15")]
            Self::Bitstring(term) => term.as_c_arg(),
            Self::Float(term) => term.as_c_arg(),
            Self::Fun(term) => term.as_c_arg(),
            Self::Integer(term) => term.as_c_arg(),
            Self::List(term) => term.as_c_arg(),
            Self::LocalPid(term) => term.as_c_arg(),
            Self::Map(term) => term.as_c_arg(),
            Self::Pid(term) => term.as_c_arg(),
            Self::Port(term) => term.as_c_arg(),
            Self::Reference(term) => term.as_c_arg(),
            Self::Tuple(term) => term.as_c_arg(),
        }
    }

    /// Associate the enum with its original environment without copying.
    ///
    /// # Safety
    ///
    /// `env` must own the term, unless this is an `Atom` or `LocalPid` variant.
    pub unsafe fn to_term(&self, env: Env<'a>) -> Term<'a> {
        BoundTerm::to_term(self, env)
    }
}

unsafe impl<'a> BoundTerm<'a> for TypedTerm<'a> {
    fn as_c_arg(&self) -> ErlNifTerm {
        self.as_c_arg()
    }

    fn from_term(term: Term<'a>) -> NifResult<Self> {
        Self::from_term(term)
    }

    unsafe fn from_raw(raw: ErlNifTerm, env: Env<'a>) -> Self {
        Self::from_term(Term::new(env, raw)).expect("unsupported raw term type")
    }
}

impl<'a> Decoder<'a> for TypedTerm<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        Self::from_term(term)
    }
}

impl Encoder for TypedTerm<'_> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            Self::Atom(term) => term.encode(env),
            Self::Binary(term) => term.encode(env),
            #[cfg(feature = "nif_version_2_15")]
            Self::Bitstring(term) => term.encode(env),
            Self::Float(term) => term.encode(env),
            Self::Fun(term) => term.encode(env),
            Self::Integer(term) => term.encode(env),
            Self::List(term) => term.encode(env),
            Self::LocalPid(term) => term.encode(env),
            Self::Map(term) => term.encode(env),
            Self::Pid(term) => term.encode(env),
            Self::Port(term) => term.encode(env),
            Self::Reference(term) => term.encode(env),
            Self::Tuple(term) => term.encode(env),
        }
    }
}

impl<'a> TryFrom<Term<'a>> for TypedTerm<'a> {
    type Error = Error;

    fn try_from(term: Term<'a>) -> Result<Self, Self::Error> {
        Self::from_term(term)
    }
}
