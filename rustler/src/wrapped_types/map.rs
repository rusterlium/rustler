//! Utilities used to access and create Erlang maps.

use crate::sys::{enif_get_map_value, enif_make_map_put, enif_make_new_map};
use crate::types::atom;
use crate::wrapper::map;
use crate::{Decoder, Encoder, Env, Error, NifResult, Term, TermType};
use std::mem::MaybeUninit;
use std::ops::RangeInclusive;

use super::wrapper;

wrapper!(
    /// A wrapper around an Erlang map term.
    struct Map(TermType::Map)
);

impl<'a> Env<'a> {
    pub fn new_map(self) -> Map<'a> {
        unsafe { Map::wrap_unchecked(Term::new(self, enif_make_new_map(self.as_c_arg()))) }
    }

    pub fn map_from_pairs(self, pairs: &[(impl Encoder, impl Encoder)]) -> NifResult<Map<'a>> {
        Term::map_from_pairs(self, pairs).map(|res| res.try_into().unwrap())
    }

    pub fn map_from_arrays(
        self,
        keys: &[impl Encoder],
        values: &[impl Encoder],
    ) -> NifResult<Map<'a>> {
        Term::map_from_arrays(self, keys, values).map(|res| res.try_into().unwrap())
    }

    pub fn map_from_term_arrays(
        self,
        keys: &[Term<'a>],
        values: &[Term<'a>],
    ) -> NifResult<Map<'a>> {
        Term::map_from_term_arrays(self, keys, values).map(|res| res.try_into().unwrap())
    }
}

impl<'a> Map<'a> {
    pub fn get(&self, key: impl Encoder) -> Option<Term<'a>> {
        let env = self.get_env();
        let key = key.encode(env);

        let mut result = MaybeUninit::uninit();
        let success = unsafe {
            enif_get_map_value(
                env.as_c_arg(),
                self.as_c_arg(),
                key.as_c_arg(),
                result.as_mut_ptr(),
            )
        };

        if success != 1 {
            return None;
        }

        unsafe { Some(Term::new(env, result.assume_init())) }
    }

    pub fn put(&self, key: impl Encoder, value: impl Encoder) -> NifResult<Self> {
        let env = self.get_env();
        let key = key.encode(env);
        let value = value.encode(env);

        let mut result = MaybeUninit::uninit();
        let success = unsafe {
            enif_make_map_put(
                env.as_c_arg(),
                self.as_c_arg(),
                key.as_c_arg(),
                value.as_c_arg(),
                result.as_mut_ptr(),
            )
        };

        if success != 1 {
            return Err(Error::BadArg);
        }
        unsafe { Ok(Map::wrap_ptr_unchecked(env, result.assume_init())) }
    }
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
        env.new_map().unwrap()
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

    /// Construct a new map from two vectors of terms.
    ///
    /// It is identical to map_from_arrays, but requires the keys and values to
    /// be encoded already - this is useful for constructing maps whose values
    /// or keys are different Rust types, with the same performance as map_from_arrays.
    pub fn map_from_term_arrays(
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

struct SimpleMapIterator<'a> {
    map: Term<'a>,
    entry: map::MapIteratorEntry,
    iter: Option<map::ErlNifMapIterator>,
    last_key: Option<Term<'a>>,
    done: bool,
}

impl<'a> SimpleMapIterator<'a> {
    fn next(&mut self) -> Option<(Term<'a>, Term<'a>)> {
        if self.done {
            return None;
        }

        let iter = loop {
            match self.iter.as_mut() {
                None => {
                    match unsafe {
                        map::map_iterator_create(
                            self.map.get_env().as_c_arg(),
                            self.map.as_c_arg(),
                            self.entry,
                        )
                    } {
                        Some(iter) => {
                            self.iter = Some(iter);
                            continue;
                        }
                        None => {
                            self.done = true;
                            return None;
                        }
                    }
                }
                Some(iter) => {
                    break iter;
                }
            }
        };

        let env = self.map.get_env();

        unsafe {
            match map::map_iterator_get_pair(env.as_c_arg(), iter) {
                Some((key, value)) => {
                    match self.entry {
                        map::MapIteratorEntry::First => {
                            map::map_iterator_next(env.as_c_arg(), iter);
                        }
                        map::MapIteratorEntry::Last => {
                            map::map_iterator_prev(env.as_c_arg(), iter);
                        }
                    }
                    let key = Term::new(env, key);
                    self.last_key = Some(key);
                    Some((key, Term::new(env, value)))
                }
                None => {
                    self.done = true;
                    None
                }
            }
        }
    }
}

impl Drop for SimpleMapIterator<'_> {
    fn drop(&mut self) {
        if let Some(iter) = self.iter.as_mut() {
            unsafe {
                map::map_iterator_destroy(self.map.get_env().as_c_arg(), iter);
            }
        }
    }
}

pub struct MapIterator<'a> {
    forward: SimpleMapIterator<'a>,
    reverse: SimpleMapIterator<'a>,
}

impl<'a> MapIterator<'a> {
    pub fn new(map: Term<'a>) -> Option<MapIterator<'a>> {
        if map.is_map() {
            Some(MapIterator {
                forward: SimpleMapIterator {
                    map,
                    entry: map::MapIteratorEntry::First,
                    iter: None,
                    last_key: None,
                    done: false,
                },
                reverse: SimpleMapIterator {
                    map,
                    entry: map::MapIteratorEntry::Last,
                    iter: None,
                    last_key: None,
                    done: false,
                },
            })
        } else {
            None
        }
    }
}

impl<'a> Iterator for MapIterator<'a> {
    type Item = (Term<'a>, Term<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        self.forward.next().and_then(|(key, value)| {
            if self.reverse.last_key == Some(key) {
                self.forward.done = true;
                self.reverse.done = true;
                return None;
            }
            Some((key, value))
        })
    }
}

impl DoubleEndedIterator for MapIterator<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.reverse.next().and_then(|(key, value)| {
            if self.forward.last_key == Some(key) {
                self.forward.done = true;
                self.reverse.done = true;
                return None;
            }
            Some((key, value))
        })
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
