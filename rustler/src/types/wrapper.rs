use crate::TermType;

struct WrapperError;

pub trait Wrapper<'a>: Sized {
    const WRAPPED_TYPE: TermType;

    fn wrap<'b>(term: Term<'a>) -> Result<Self, WrapperError> {
        if term.get_type() == Self::WRAPPED_TYPE {
            unsafe { Ok(Self::wrap_unsafe(term)) }
        } else {
            Err(WrapperError)
        }
    }

    fn unwrap(&self) -> Term<'a>;

    unsafe fn wrap_unsafe(term: Term<'a>) -> Self;

    /// Returns a representation of self in the given Env.
    ///
    /// If the term is already is in the provided env, it will be directly returned. Otherwise
    /// the term will be copied over.
    fn in_env<'b>(&self, env: Env<'b>) -> impl Wrapper<'b, WRAPPED_TYPE = Self::WrappedType> {
        self.unwrap().in_env(env)
    }
}

macro_rules! wrapper {
    ($name:ident, $term_type:path) => {
        #[derive(PartialEq, Eq, Clone, Copy)]
        pub struct $name<'a>(Term<'a>);

        impl<'a> $crate::types::wrapper::Wrapper<'a> for $name<'a> {
            const WrappedType: TermType = $term_type;

            unsafe fn wrap_unsafe(term: Term<'a>) -> Self {
                $name(term)
            }

            fn unwrap(&self) -> Term<'a> {
                self.0
            }
        }
    };
}

pub(crate) use wrapper;

use crate::{Env, Term};
