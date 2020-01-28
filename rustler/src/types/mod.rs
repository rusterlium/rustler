use crate::{Env, Error, NifResult, Term};

#[macro_use]
pub mod atom;
pub use crate::types::atom::Atom;

#[doc(hidden)]
pub mod binary;
pub use crate::types::binary::{Binary, OwnedBinary};

#[doc(hidden)]
pub mod list;
pub use crate::types::list::ListIterator;

#[doc(hidden)]
pub mod map;
pub use self::map::MapIterator;

#[doc(hidden)]
pub mod primitive;
#[doc(hidden)]
pub mod string;
pub mod tuple;

#[doc(hidden)]
pub mod local_pid;
pub use self::local_pid::LocalPid;

#[deprecated(since = "0.22.0", note = "Please use local_pid instead")]
pub mod pid {
    #[deprecated(since = "0.22.0", note = "Please use LocalPid instead")]
    pub use super::LocalPid as Pid;
}
#[deprecated(since = "0.22.0", note = "Please use LocalPid instead")]
pub use self::LocalPid as Pid;

pub mod truthy;

pub mod elixir_struct;

pub trait Encoder {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a>;
}
pub trait Decoder<'a>: Sized + 'a {
    fn decode(term: Term<'a>) -> NifResult<Self>;
}

impl<'a> Encoder for Term<'a> {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.in_env(env)
    }
}
impl<'a> Decoder<'a> for Term<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        Ok(term)
    }
}

impl<'a, T> Encoder for &'a T
where
    T: Encoder,
{
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        <T as Encoder>::encode(self, env)
    }
}

impl<T> Encoder for Option<T>
where
    T: Encoder,
{
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        match *self {
            Some(ref value) => value.encode(env),
            None => atom::nil().encode(env),
        }
    }
}

impl<'a, T> Decoder<'a> for Option<T>
where
    T: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        if let Ok(term) = term.decode::<T>() {
            Ok(Some(term))
        } else {
            let decoded_atom: atom::Atom = term.decode()?;
            if decoded_atom == atom::nil() {
                Ok(None)
            } else {
                Err(Error::BadArg)
            }
        }
    }
}

impl<T, E> Encoder for Result<T, E>
where
    T: Encoder,
    E: Encoder,
{
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        match *self {
            Ok(ref value) => (atom::ok().encode(env), value.encode(env)).encode(env),
            Err(ref err) => (atom::error().encode(env), err.encode(env)).encode(env),
        }
    }
}

impl<'a, T, E> Decoder<'a> for Result<T, E>
where
    T: Decoder<'a>,
    E: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let (decoded_atom, inner_term): (atom::Atom, Term) = term.decode()?;
        if decoded_atom == atom::ok() {
            let ok_value: T = inner_term.decode()?;
            Ok(Ok(ok_value))
        } else if decoded_atom == atom::error() {
            let err_value: E = inner_term.decode()?;
            Ok(Err(err_value))
        } else {
            Err(Error::BadArg)
        }
    }
}
