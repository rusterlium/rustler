use crate::{types::atom, Env, Term};
use std::panic::RefUnwindSafe;

pub trait Encoder {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a>;

    /// Erase this value's type and encode it later, when an environment is available.
    fn defer<'a>(self) -> DeferredTerm<'a>
    where
        Self: Sized + RefUnwindSafe + 'a,
    {
        DeferredTerm::new(self)
    }
}

/// An encodable value whose concrete type is erased until it is encoded.
///
/// Unlike [`Term`], this need not contain an Erlang term or belong to an environment.
/// It can hold borrowed values, including environment-bound terms, without outliving them.
/// Values must be [`RefUnwindSafe`] so the NIF return path can catch encoding panics.
///
/// Return an encodable value directly when its type is known; use `DeferredTerm` when
/// different branches or collection elements have different types.
///
/// ```
/// use rustler::{DeferredTerm, Encoder};
///
/// fn choose(flag: bool) -> DeferredTerm<'static> {
///     if flag {
///         42_i32.defer()
///     } else {
///         "hello".defer()
///     }
/// }
/// ```
///
/// ```compile_fail
/// use rustler::{DeferredTerm, Term};
///
/// fn escape<'a>(term: Term<'a>) -> DeferredTerm<'static> {
///     DeferredTerm::new(term)
/// }
/// ```
pub struct DeferredTerm<'a>(Box<dyn Encoder + RefUnwindSafe + 'a>);

impl<'a> DeferredTerm<'a> {
    /// Store a value for encoding in a later environment.
    pub fn new<T: Encoder + RefUnwindSafe + 'a>(value: T) -> Self {
        Self(Box::new(value))
    }
}

impl Encoder for DeferredTerm<'_> {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        self.0.encode(env)
    }
}

impl Encoder for Term<'_> {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.in_env(env)
    }
}

impl<T> Encoder for &T
where
    T: Encoder,
{
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        <T as Encoder>::encode(self, env)
    }
}

impl<T> Encoder for Box<T>
where
    T: Encoder,
{
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        self.as_ref().encode(env)
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

impl<K, V> Encoder for std::collections::HashMap<K, V>
where
    K: Encoder + Eq + std::hash::Hash,
    V: Encoder,
{
    fn encode<'c>(&self, env: Env<'c>) -> Term<'c> {
        let (keys, values): (Vec<_>, Vec<_>) = self.iter().unzip();
        Term::map_from_arrays(env, &keys, &values).unwrap()
    }
}
