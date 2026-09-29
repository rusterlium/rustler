//! Utilities used to access and create Erlang maps.

use super::atom;
use crate::sys::{
    enif_get_map_size, enif_get_map_value, enif_make_map_from_arrays, enif_make_map_put,
    enif_make_map_remove, enif_make_map_update, enif_make_new_map, enif_map_iterator_create,
    enif_map_iterator_destroy, enif_map_iterator_get_pair, enif_map_iterator_next,
    enif_map_iterator_prev, ErlNifEnv, ErlNifMapIterator, ErlNifMapIteratorEntry, ErlNifTerm,
};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::mem::MaybeUninit;
use std::ops::RangeInclusive;

#[inline]
pub fn map_new(env: Env) -> Term {
    unsafe { Term::new(env, enif_make_new_map(env.as_c_arg())) }
}

/// ## Map terms
impl<'a> Term<'a> {
    /// Constructs a new, empty map term.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// %{}
    /// ```
    #[inline]
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
    #[inline]
    pub fn map_from_arrays(
        env: Env<'a>,
        keys: &[impl Encoder],
        values: &[impl Encoder],
    ) -> NifResult<Term<'a>> {
        if keys.len() == values.len() {
            let keys: Vec<_> = keys.iter().map(|k| k.encode(env).as_c_arg()).collect();
            let values: Vec<_> = values.iter().map(|v| v.encode(env).as_c_arg()).collect();

            unsafe {
                make_map_from_arrays(env.as_c_arg(), &keys, &values)
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
                make_map_from_arrays(env.as_c_arg(), &keys, &values)
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
    #[inline]
    pub fn map_from_pairs(
        env: Env<'a>,
        pairs: &[(impl Encoder, impl Encoder)],
    ) -> NifResult<Term<'a>> {
        let (keys, values): (Vec<_>, Vec<_>) = pairs
            .iter()
            .map(|(k, v)| (k.encode(env).as_c_arg(), v.encode(env).as_c_arg()))
            .unzip();

        unsafe {
            make_map_from_arrays(env.as_c_arg(), &keys, &values)
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
    #[inline]
    pub fn map_get(self, key: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();
        match unsafe { get_map_value(env.as_c_arg(), self.as_c_arg(), key.encode(env).as_c_arg()) }
        {
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
    #[inline]
    pub fn map_size(self) -> NifResult<usize> {
        let env = self.get_env();
        unsafe { get_map_size(env.as_c_arg(), self.as_c_arg()).ok_or(Error::BadArg) }
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
    #[inline]
    pub fn map_put(self, key: impl Encoder, value: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();

        match unsafe {
            map_put(
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
    #[inline]
    pub fn map_remove(self, key: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();

        match unsafe { map_remove(env.as_c_arg(), self.as_c_arg(), key.encode(env).as_c_arg()) } {
            Some(inner) => Ok(unsafe { Term::new(env, inner) }),
            None => Err(Error::BadArg),
        }
    }

    /// Makes a copy of the self map term where key is set to value.
    ///
    /// Returns Err(Error::BadArg) if the term is not a map of if key
    /// doesn't exist.
    #[inline]
    pub fn map_update(self, key: impl Encoder, new_value: impl Encoder) -> NifResult<Term<'a>> {
        let env = self.get_env();

        match unsafe {
            map_update(
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
    entry: MapIteratorEntry,
    iter: Option<ErlNifMapIterator>,
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
                        map_iterator_create(
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
            match map_iterator_get_pair(env.as_c_arg(), iter) {
                Some((key, value)) => {
                    match self.entry {
                        MapIteratorEntry::First => {
                            map_iterator_next(env.as_c_arg(), iter);
                        }
                        MapIteratorEntry::Last => {
                            map_iterator_prev(env.as_c_arg(), iter);
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
                map_iterator_destroy(self.map.get_env().as_c_arg(), iter);
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
                    entry: MapIteratorEntry::First,
                    iter: None,
                    last_key: None,
                    done: false,
                },
                reverse: SimpleMapIterator {
                    map,
                    entry: MapIteratorEntry::Last,
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

#[derive(Clone, Copy, Debug)]
enum MapIteratorEntry {
    First,
    Last,
}

unsafe fn get_map_value(
    env: *mut ErlNifEnv,
    map: ErlNifTerm,
    key: ErlNifTerm,
) -> Option<ErlNifTerm> {
    let mut result = MaybeUninit::uninit();
    let success = enif_get_map_value(env, map, key, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

unsafe fn get_map_size(env: *mut ErlNifEnv, map: ErlNifTerm) -> Option<usize> {
    let mut size = MaybeUninit::uninit();
    let success = enif_get_map_size(env, map, size.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(size.assume_init())
}

unsafe fn map_put(
    env: *mut ErlNifEnv,
    map: ErlNifTerm,
    key: ErlNifTerm,
    value: ErlNifTerm,
) -> Option<ErlNifTerm> {
    let mut result = MaybeUninit::uninit();
    let success = enif_make_map_put(env, map, key, value, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

unsafe fn map_remove(env: *mut ErlNifEnv, map: ErlNifTerm, key: ErlNifTerm) -> Option<ErlNifTerm> {
    let mut result = MaybeUninit::uninit();
    let success = enif_make_map_remove(env, map, key, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

unsafe fn map_update(
    env: *mut ErlNifEnv,
    map: ErlNifTerm,
    key: ErlNifTerm,
    new_value: ErlNifTerm,
) -> Option<ErlNifTerm> {
    let mut result = MaybeUninit::uninit();
    let success = enif_make_map_update(env, map, key, new_value, result.as_mut_ptr());

    if success != 1 {
        return None;
    }
    Some(result.assume_init())
}

unsafe fn map_iterator_create(
    env: *mut ErlNifEnv,
    map: ErlNifTerm,
    entry: MapIteratorEntry,
) -> Option<ErlNifMapIterator> {
    let mut iter = MaybeUninit::uninit();
    let success = enif_map_iterator_create(
        env,
        map,
        iter.as_mut_ptr(),
        match entry {
            MapIteratorEntry::First => ErlNifMapIteratorEntry::ERL_NIF_MAP_ITERATOR_HEAD,
            MapIteratorEntry::Last => ErlNifMapIteratorEntry::ERL_NIF_MAP_ITERATOR_TAIL,
        },
    );
    if success == 0 {
        None
    } else {
        Some(iter.assume_init())
    }
}

unsafe fn map_iterator_destroy(env: *mut ErlNifEnv, iter: &mut ErlNifMapIterator) {
    enif_map_iterator_destroy(env, iter);
}

unsafe fn map_iterator_get_pair(
    env: *mut ErlNifEnv,
    iter: &mut ErlNifMapIterator,
) -> Option<(ErlNifTerm, ErlNifTerm)> {
    let mut key = MaybeUninit::uninit();
    let mut value = MaybeUninit::uninit();
    if enif_map_iterator_get_pair(env, iter, key.as_mut_ptr(), value.as_mut_ptr()) == 0 {
        None
    } else {
        Some((key.assume_init(), value.assume_init()))
    }
}

#[inline]
unsafe fn map_iterator_next(env: *mut ErlNifEnv, iter: &mut ErlNifMapIterator) {
    enif_map_iterator_next(env, iter);
}

#[inline]
unsafe fn map_iterator_prev(env: *mut ErlNifEnv, iter: &mut ErlNifMapIterator) {
    enif_map_iterator_prev(env, iter);
}

#[inline]
unsafe fn make_map_from_arrays(
    env: *mut ErlNifEnv,
    keys: &[ErlNifTerm],
    values: &[ErlNifTerm],
) -> Option<ErlNifTerm> {
    let mut map = MaybeUninit::uninit();
    if enif_make_map_from_arrays(
        env,
        keys.as_ptr(),
        values.as_ptr(),
        keys.len(),
        map.as_mut_ptr(),
    ) == 0
    {
        return None;
    }

    Some(map.assume_init())
}
