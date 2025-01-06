use crate::sys::ERL_NIF_TERM;
use crate::{Env, Term, TermType};

pub struct WrapperError;

impl From<WrapperError> for crate::Error {
    fn from(_: WrapperError) -> Self {
        crate::Error::BadArg
    }
}

pub(crate) trait Wrapper<'a>: Sized {
    const WRAPPED_TYPE: TermType;

    unsafe fn wrap_ptr_unchecked(env: Env<'a>, ptr: ERL_NIF_TERM) -> Self {
        Self::wrap_unchecked(Term::new(env, ptr))
    }

    fn wrap(term: Term<'a>) -> Result<Self, WrapperError> {
        if term.get_type() == Self::WRAPPED_TYPE {
            unsafe { Ok(Self::wrap_unchecked(term)) }
        } else {
            Err(WrapperError)
        }
    }

    fn unwrap(&self) -> Term<'a>;
    unsafe fn wrap_unchecked(term: Term<'a>) -> Self;
}

impl<'a, T> From<T> for Term<'a>
where
    T: Wrapper<'a>,
{
    fn from(term: T) -> Self {
        term.unwrap()
    }
}

macro_rules! wrapper {
    (
        $(#[$meta:meta])*
        struct $name:ident($term_type:path)
    ) => {
        $(#[$meta])*
        #[derive(PartialEq, Eq, Clone, Copy)]
        pub struct $name<'a>(Term<'a>);

        use $crate::wrapped_types::Wrapper;

        impl<'a> $name<'a> {
            /// Returns a representation of self in the given Env.
            ///
            /// If the term is already is in the provided env, it will be directly returned.
            /// Otherwise the term will be copied over.
            pub fn in_env<'b>(&self, env: Env<'b>) -> $name<'b> {
                let term = self.unwrap().in_env(env);
                unsafe { $name::wrap_unchecked(term) }
            }
        }

        impl<'a> Wrapper<'a> for $name<'a> {
            const WRAPPED_TYPE: $crate::TermType = $term_type;

            unsafe fn wrap_unchecked(term: Term<'a>) -> Self {
                $name(term)
            }

            fn unwrap(&self) -> Term<'a> {
                self.0
            }
        }

        impl<'a> std::ops::Deref for $name<'a> {
            type Target = Term<'a>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'a> TryFrom<Term<'a>> for $name<'a> {
            type Error = $crate::Error;

            fn try_from(term: Term<'a>) -> Result<Self, Self::Error> {
                use $crate::wrapped_types::Wrapper;
                Self::wrap(term).or(Err($crate::Error::BadArg))
            }
        }

        impl<'a> $crate::Decoder<'a> for $name<'a> {
            fn decode(term: Term<'a>) -> $crate::NifResult<Self> {
                use $crate::wrapped_types::Wrapper;
                Self::wrap(term).or(Err($crate::Error::BadArg))
            }
        }

        impl<'a> $crate::Encoder for $name<'a> {
            fn encode<'b>(&self, env: $crate::Env<'b>) -> Term<'b> {
                self.0.encode(env)
            }
        }
    };
}

pub(crate) use wrapper;
