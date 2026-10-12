use crate::{types::atom, Atom, Error, MapIterator, NifResult, Term};

pub trait Decoder<'a>: Sized + 'a {
    fn decode(term: Term<'a>) -> NifResult<Self>;
}

impl<'a> Decoder<'a> for Term<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        Ok(term)
    }
}

impl<'a, T> Decoder<'a> for Box<T>
where
    T: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        term.decode().map(Box::new)
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
            let decoded_atom: Atom = term.decode()?;
            if decoded_atom == atom::nil() {
                Ok(None)
            } else {
                Err(Error::BadArg)
            }
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

impl<'a, K, V> Decoder<'a> for std::collections::HashMap<K, V>
where
    K: Decoder<'a> + Eq + std::hash::Hash,
    V: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let size = term.map_size()?;

        let it = MapIterator::new(term).ok_or(Error::BadArg)?;

        let mut map = std::collections::HashMap::with_capacity(size);

        for (k, v) in it {
            let k = k.decode()?;
            let v = v.decode()?;
            map.insert(k, v);
        }

        Ok(map)
    }
}
