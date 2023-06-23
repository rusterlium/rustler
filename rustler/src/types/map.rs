//! Utilities used to access and create Erlang maps.

use super::atom;
use crate::wrapper::map;
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::ops::RangeInclusive;

pub fn map_new(env: Env) -> Term {
    unsafe { Term::new(env, map::map_new(env.as_c_arg())) }
}

/// ## Map terms
impl<'a> Term<'a> {
    /// Constructs a new, empty map term.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// %{}
    /// ```
    pub fn map_new(env: Env<'a>) -> Term<'a> {
        map_new(env)
    }

    /// Construct a new map from two vectors
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// keys = ["foo", "bar"]
    /// values = [1, 2]
    /// Enum.zip(keys, values) |> Map.new()
    /// ```
    pub fn map_from_arrays(
        env: Env<'a>,
        keys: &[impl Encoder],
        values: &[impl Encoder],
    ) -> NifResult<Term<'a>> {
        if keys.len() == values.len() {
            let keys: Vec<_> = keys.iter().map(|k| k.encode(env).as_c_arg()).collect();
            let values: Vec<_> = values.iter().map(|v| v.encode(env).as_c_arg()).collect();

            unsafe {
                map::make_map_from_arrays(env.as_c_arg(), &keys, &values)
                    .map_or_else(|| Err(Error::BadArg), |map| Ok(Term::new(env, map)))
            }
        } else {
            Err(Error::BadArg)
        }
    }

    /// Construct a new map from two iterables
    ///
    /// It is identical to map_from_arrays, but accepts tuples
    /// instead of arrays to allow differing types within the
    /// keys and values.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// keys = ["foo", "bar"]
    /// values = [1, true]
    /// Enum.zip(values) |> Map.new()
    /// ```
    pub fn map_from_iterables(
        env: Env<'a>,
        keys: impl EncoderIterable<'a>,
        values: impl EncoderIterable<'a>,
    ) -> NifResult<Term<'a>> {
        let keys: Vec<_> = keys.terms(env).into_iter().map(|t| t.as_c_arg()).collect();
        let values: Vec<_> = values.terms(env).into_iter().map(|t| t.as_c_arg()).collect();

        if keys.len() == values.len() {
            unsafe {
                map::make_map_from_arrays(env.as_c_arg(), &keys, &values)
                    .map_or_else(|| Err(Error::BadArg), |map| Ok(Term::new(env, map)))
            }
        } else {
            Err(Error::BadArg)
        }
    }

    /// Construct a new map from pairs of terms
    ///
    /// It is similar to `map_from_arrays` but
    /// receives only one vector with the pairs
    /// of `(key, value)`.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// Map.new([{"foo", 1}, {"bar", 2}])
    /// ```
    pub fn map_from_pairs(
        env: Env<'a>,
        pairs: &[(impl Encoder, impl Encoder)],
    ) -> NifResult<Term<'a>> {
        let (keys, values): (Vec<_>, Vec<_>) = pairs
            .iter()
            .map(|(k, v)| (k.encode(env).as_c_arg(), v.encode(env).as_c_arg()))
            .unzip();

        unsafe {
            map::make_map_from_arrays(env.as_c_arg(), &keys, &values)
                .map_or_else(|| Err(Error::BadArg), |map| Ok(Term::new(env, map)))
        }
    }

    /// Gets the value corresponding to a key in a map term.
    ///
    /// Returns Err(Error::BadArg) if the term is not a map or if
    /// key doesn't exist in the map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// Map.get(self_term, key)
    /// ```
    pub fn map_get(self, key: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();
        match unsafe {
            map::get_map_value(env.as_c_arg(), self.as_c_arg(), key.encode(env).as_c_arg())
        } {
            Some(value) => Ok(unsafe { Term::new(env, value) }),
            None => Err(Error::BadArg),
        }
    }

    /// Gets the size of a map term.
    ///
    /// Returns Err(Error::BadArg) if the term is not a map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// map_size(self_term)
    /// ```
    pub fn map_size(self) -> NifResult<usize> {
        let env = self.get_env();
        unsafe { map::get_map_size(env.as_c_arg(), self.as_c_arg()).ok_or(Error::BadArg) }
    }

    /// Makes a copy of the self map term and sets key to value.
    /// If the value already exists, it is overwritten.
    ///
    /// Returns Err(Error::BadArg) if the term is not a map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// Map.put(self_term, key, value)
    /// ```
    pub fn map_put(self, key: impl Encoder, value: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();

        match unsafe {
            map::map_put(
                env.as_c_arg(),
                self.as_c_arg(),
                key.encode(env).as_c_arg(),
                value.encode(env).as_c_arg(),
            )
        } {
            Some(inner) => Ok(unsafe { Term::new(env, inner) }),
            None => Err(Error::BadArg),
        }
    }

    /// Makes a copy of the self map term and removes key. If the key
    /// doesn't exist, the original map is returned.
    ///
    /// Returns Err(Error::BadArg) if the term is not a map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// Map.delete(self_term, key)
    /// ```
    pub fn map_remove(self, key: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();

        match unsafe {
            map::map_remove(env.as_c_arg(), self.as_c_arg(), key.encode(env).as_c_arg())
        } {
            Some(inner) => Ok(unsafe { Term::new(env, inner) }),
            None => Err(Error::BadArg),
        }
    }

    /// Makes a copy of the self map term where key is set to value.
    ///
    /// Returns Err(Error::BadArg) if the term is not a map of if key
    /// doesn't exist.
    pub fn map_update(self, key: impl Encoder, new_value: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();

        match unsafe {
            map::map_update(
                env.as_c_arg(),
                self.as_c_arg(),
                key.encode(env).as_c_arg(),
                new_value.encode(env).as_c_arg(),
            )
        } {
            Some(inner) => Ok(unsafe { Term::new(env, inner) }),
            None => Err(Error::BadArg),
        }
    }
}

pub struct MapIterator<'a> {
    env: Env<'a>,
    iter: map::ErlNifMapIterator,
}

impl<'a> MapIterator<'a> {
    pub fn new(map: Term<'a>) -> Option<MapIterator<'a>> {
        let env = map.get_env();
        unsafe { map::map_iterator_create(env.as_c_arg(), map.as_c_arg()) }
            .map(|iter| MapIterator { env, iter })
    }
}

impl<'a> Drop for MapIterator<'a> {
    fn drop(&mut self) {
        unsafe {
            map::map_iterator_destroy(self.env.as_c_arg(), &mut self.iter);
        }
    }
}

impl<'a> Iterator for MapIterator<'a> {
    type Item = (Term<'a>, Term<'a>);

    fn next(&mut self) -> Option<(Term<'a>, Term<'a>)> {
        unsafe {
            map::map_iterator_get_pair(self.env.as_c_arg(), &mut self.iter).map(|(key, value)| {
                map::map_iterator_next(self.env.as_c_arg(), &mut self.iter);
                (Term::new(self.env, key), Term::new(self.env, value))
            })
        }
    }
}

impl<'a> Decoder<'a> for MapIterator<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        match MapIterator::new(term) {
            Some(iter) => Ok(iter),
            None => Err(Error::BadArg),
        }
    }
}

impl<'a, T> Decoder<'a> for RangeInclusive<T>
where
    T: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let name = term.map_get(atom::__struct__())?;

        match name.atom_to_string()?.as_ref() {
            "Elixir.Range" => (),
            _ => return Err(Error::BadArg),
        }

        let first = term.map_get(atom::first())?.decode::<T>()?;
        let last = term.map_get(atom::last())?.decode::<T>()?;
        if let Ok(step) = term.map_get(atom::step()) {
            match step.decode::<i64>()? {
                1 => (),
                _ => return Err(Error::BadArg),
            }
        }

        Ok(first..=last)
    }
}

/// A trait for types that can be converted to an iterable of terms
/// using an `Env`.
///
/// Used by Term::map_from_iterables
pub trait EncoderIterable<'a> {
    type IntoIter: IntoIterator<Item = Term<'a>>;

    fn terms(&self, env: Env<'a>) -> Self::IntoIter;
}

/// Helper macro that returns the number of comma-separated expressions passed to it.
/// For example, `count!(a + b, c)` evaluates to `2`.
macro_rules! count {
    ( ) => ( 0 );
    ( $blah:expr ) => ( 1 );
    ( $blah:expr, $( $others:expr ),* ) => ( 1 + count!( $( $others ),* ) )
}

macro_rules! impl_encoder_args {
    ( $($index:tt : $tyvar:ident),* ) => (
        impl<'a, $( $tyvar: Encoder ),*>
            EncoderIterable<'a> for ( $( $tyvar ),* ) {

            type IntoIter = [Term<'a>; count!( $( $tyvar ),* )];

            fn terms(&self, env: Env<'a>) -> Self::IntoIter {
                [ $( Encoder::encode(&self.$index, env) ),* ]
            }
        }
    );
}

impl <'a, A:Encoder> EncoderIterable<'a> for (A,) {
    type IntoIter = [Term<'a>; 1];

    fn terms(&self,env:Env<'a>) -> Self::IntoIter {
        [Encoder::encode(&self.0, env)]
    }
}
impl_encoder_args!(0: A, 1: B);
impl_encoder_args!(0: A, 1: B, 2: C);
impl_encoder_args!(0: A, 1: B, 2: C, 3: D);
impl_encoder_args!(0: A, 1: B, 2: C, 3: D, 4: E);
impl_encoder_args!(0: A, 1: B, 2: C, 3: D, 4: E, 5: F);
impl_encoder_args!(0: A, 1: B, 2: C, 3: D, 4: E, 5: F, 6: G);
