use crate::sys::ERL_NIF_TERM;
use crate::{Env, Term, TermType};

pub struct WrapperError;

impl From<WrapperError> for crate::Error {
    fn from(_: WrapperError) -> Self {
        crate::Error::BadArg
    }
}

pub trait Wrapper: Sized {
    const WRAPPED_TYPE: TermType;

    unsafe fn wrap_ptr_unchecked<'a>(env: Env<'a>, ptr: ERL_NIF_TERM) -> Self {
        Self::wrap_unchecked(Term::new(env, ptr))
    }

    fn wrap<'a>(term: Term<'a>) -> Result<Self, WrapperError> {
        if term.get_type() == Self::WRAPPED_TYPE {
            unsafe { Ok(Self::wrap_unchecked(term)) }
        } else {
            Err(WrapperError)
        }
    }

    fn unwrap<'a>(&self, env: Env<'a>) -> Term<'a>;

    unsafe fn unwrap_ptr_unchecked<'a>(&self, env: Env<'a>) -> ERL_NIF_TERM {
        self.unwrap(env).as_c_arg()
    }
    unsafe fn wrap_unchecked<'a>(term: Term<'a>) -> Self;
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
                let term = self.unwrap(env);
                unsafe { $name::wrap_unchecked(term) }
            }
        }

        impl<'a> Wrapper for $name<'a> {
            const WRAPPED_TYPE: $crate::TermType = $term_type;

            unsafe fn wrap_unchecked<'b>(term: Term<'b>) -> Self {
                $name(term)
            }

            fn unwrap<'b>(&self, env: Env<'b>) -> Term<'b> {
                self.0.in_env(env)
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
                self.unwrap().encode(env)
            }
        }

        // impl<'a> std::hash::Hash for $name<'a> {
        //     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        //         self.0.hash(state);
        //     }
        // }
        //
        impl<'a> std::fmt::Debug for $name<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                self.0.fmt(f)
            }
        }
    };
}

macro_rules! wrapper_impls {
    ($name: ident, $term_type:path) => {
        impl<'a> $name<'a> {
            /// Returns a representation of self in the given Env.
            ///
            /// If the term is already is in the provided env, it will be directly returned.
            /// Otherwise the term will be copied over.
            pub fn in_env<'b>(&self, env: Env<'b>) -> $name<'b> {
                use $crate::wrapped_types::Wrapper;
                let term = self.unwrap().in_env(env);
                unsafe { $name::wrap_unchecked(term) }
            }
        }

        impl<'a> std::ops::Deref for $name<'a> {
            type Target = Term<'a>;

            fn deref(&self) -> &Self::Target {
                &self.unwrap()
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
                self.unwrap().encode(env)
            }
        }

        impl<'a> std::hash::Hash for $name<'a> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.unwrap().hash(state);
            }
        }

        impl<'a> std::fmt::Debug for $name<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                self.unwrap().fmt(f)
            }
        }
    };
}

pub(crate) use wrapper;
