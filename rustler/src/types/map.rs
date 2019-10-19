//! Utilities used to access and create Erlang maps.

use super::atom;
use crate::wrapper::map;
use crate::{Decoder, Env, Error, NifResult, Term};
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
    /// List.zip(keys, values) |> Map.new()
    /// ```
    #[cfg(nif_version_2_14)]
    pub fn map_from_arrays(
        env: Env<'a>,
        keys: &[Term<'a>],
        values: &[Term<'a>],
    ) -> NifResult<Term<'a>> {
        if keys.len() == values.len() {
            let keys: Vec<_> = keys.iter().map(|k| k.as_c_arg()).collect();
            let values: Vec<_> = values.iter().map(|v| v.as_c_arg()).collect();

            unsafe {
                map::make_map_from_arrays(env.as_c_arg(), &keys, &values)
                    .map_or_else(|| Err(Error::BadArg), |map| Ok(Term::new(env, map)))
            }
        } else {
            Err(Error::BadArg)
        }
    }

    // Fallback for older NIF version
    #[cfg(not(nif_version_2_14))]
    pub fn map_from_arrays(
        env: Env<'a>,
        keys: &[Term<'a>],
        values: &[Term<'a>],
    ) -> NifResult<Term<'a>> {
        if keys.len() == values.len() {
            let map = map_new(env);
            keys.iter()
                .zip(values.iter())
                .try_fold(map, |map, (k, v)| map.map_put(*k, *v))
        } else {
            Err(Error::BadArg)
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
    pub fn map_get(self, key: Term) -> NifResult<Term<'a>> {
        let env = self.get_env();
        match unsafe { map::get_map_value(env.as_c_arg(), self.as_c_arg(), key.as_c_arg()) } {
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
    pub fn map_put(self, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
        let map_env = self.get_env();

        assert!(
            map_env == key.get_env(),
            "key is from different environment as map"
        );
        assert!(
            map_env == value.get_env(),
            "value is from different environment as map"
        );

        match unsafe {
            map::map_put(
                map_env.as_c_arg(),
                self.as_c_arg(),
                key.as_c_arg(),
                value.as_c_arg(),
            )
        } {
            Some(inner) => Ok(unsafe { Term::new(map_env, inner) }),
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
    pub fn map_remove(self, key: Term<'a>) -> NifResult<Term<'a>> {
        let map_env = self.get_env();

        assert!(
            map_env == key.get_env(),
            "key is from different environment as map"
        );

        match unsafe { map::map_remove(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg()) } {
            Some(inner) => Ok(unsafe { Term::new(map_env, inner) }),
            None => Err(Error::BadArg),
        }
    }

    /// Makes a copy of the self map term where key is set to value.
    ///
    /// Returns Err(Error::BadArg) if the term is not a map of if key
    /// doesn't exist.
    pub fn map_update(self, key: Term<'a>, new_value: Term<'a>) -> NifResult<Term<'a>> {
        let map_env = self.get_env();

        assert!(
            map_env == key.get_env(),
            "key is from different environment as map"
        );
        assert!(
            map_env == new_value.get_env(),
            "value is from different environment as map"
        );

        match unsafe {
            map::map_update(
                map_env.as_c_arg(),
                self.as_c_arg(),
                key.as_c_arg(),
                new_value.as_c_arg(),
            )
        } {
            Some(inner) => Ok(unsafe { Term::new(map_env, inner) }),
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
        let env = term.get_env();
        let name = term.map_get(atom::__struct__().to_term(env))?;

        match name.atom_to_string()?.as_ref() {
            "Elixir.Range" => (),
            _ => return Err(Error::BadArg),
        }

        let first = term.map_get(atom::first().to_term(env))?.decode::<T>()?;
        let last = term.map_get(atom::last().to_term(env))?.decode::<T>()?;

        Ok(first..=last)
    }
}
