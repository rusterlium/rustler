use crate::{types::atom, Env, Term};

pub trait Encoder {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a>;
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
