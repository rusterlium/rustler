use crate::sys::ErlNifTerm;
use crate::{Env, FreeTerm, NifResult, Term};

/// A typed term that remains valid for an environment's lifetime.
///
/// Raw wrappers retain a lifetime, not an environment pointer. Converting them back into a
/// [`Term`] therefore requires the caller to supply the correct environment.
///
/// # Safety
///
/// Implementations must contain a valid term of their declared type throughout `'a`.
pub unsafe trait BoundTerm<'a>: Copy + 'a {
    /// Return the raw Erlang term.
    fn as_c_arg(&self) -> ErlNifTerm;

    /// Check the term's type and wrap it for its environment's lifetime.
    fn from_term(term: Term<'a>) -> NifResult<Self>;

    /// Wrap a raw Erlang term.
    ///
    /// # Safety
    ///
    /// `raw` must be a valid term of this wrapper's type belonging to `env`, unless it is
    /// environment-independent. `env` must remain valid throughout `'a`.
    unsafe fn from_raw(raw: ErlNifTerm, env: Env<'a>) -> Self;

    /// Associate the wrapper with its original environment.
    ///
    /// # Safety
    ///
    /// `env` must own this term, unless the term is environment-independent.
    /// An equal lifetime alone does not prove that two environments are the same.
    unsafe fn to_term(&self, env: Env<'a>) -> Term<'a> {
        Term::new(env, self.as_c_arg())
    }
}

unsafe impl<'a, T: FreeTerm> BoundTerm<'a> for T {
    fn as_c_arg(&self) -> ErlNifTerm {
        FreeTerm::as_c_arg(self)
    }

    fn from_term(term: Term<'a>) -> NifResult<Self> {
        FreeTerm::from_term(term)
    }

    unsafe fn from_raw(raw: ErlNifTerm, _env: Env<'a>) -> Self {
        FreeTerm::from_raw(raw)
    }
}
