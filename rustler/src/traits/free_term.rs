use crate::sys::ErlNifTerm;
use crate::{Env, NifResult, Term};

/// A typed term that does not depend on the lifetime of an environment.
///
/// Every `FreeTerm` also implements [`crate::BoundTerm`] for any lifetime.
///
/// # Safety
///
/// Implementations must contain a valid, environment-independent Erlang term, even after the
/// environment used to decode it has been cleared or freed.
pub unsafe trait FreeTerm: Copy + 'static {
    /// Return the raw Erlang term.
    fn as_c_arg(&self) -> ErlNifTerm;

    /// Check the term's type and wrap it without retaining its environment.
    fn from_term(term: Term<'_>) -> NifResult<Self>;

    /// Wrap a raw Erlang term.
    ///
    /// # Safety
    ///
    /// `raw` must be a valid, environment-independent term of this wrapper's type.
    unsafe fn from_raw(raw: ErlNifTerm) -> Self;

    /// Use this term in any environment without copying it.
    fn in_env<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe { Term::new(env, self.as_c_arg()) }
    }
}
