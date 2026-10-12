//! Map terms.

use crate::sys::{
    enif_get_map_size, enif_get_map_value, enif_make_map_from_arrays, enif_make_map_put,
    enif_make_map_remove, enif_make_map_update, enif_make_new_map, ErlNifEnv, ErlNifTerm,
};
use crate::{BoundTerm, Encoder, Env, Error, MapIterator, NifResult, Term};
use std::mem::MaybeUninit;

bound_term!(Map, crate::Term::is_map);

impl<'a> Map<'a> {
    /// Construct an empty map.
    pub fn new(env: Env<'a>) -> Self {
        unsafe { Self::from_raw(enif_make_new_map(env.as_c_arg()), env) }
    }

    /// Construct a map from equally sized key and value arrays.
    pub fn from_arrays(
        env: Env<'a>,
        keys: &[impl Encoder],
        values: &[impl Encoder],
    ) -> NifResult<Self> {
        if keys.len() != values.len() {
            return Err(Error::BadArg);
        }
        let keys: Vec<_> = keys.iter().map(|key| key.encode(env).as_c_arg()).collect();
        let values: Vec<_> = values
            .iter()
            .map(|value| value.encode(env).as_c_arg())
            .collect();
        Self::from_raw_arrays(env, &keys, &values)
    }

    /// Construct a map from term arrays, copying inputs into `env` when needed.
    pub fn from_term_arrays(
        env: Env<'a>,
        keys: &[Term<'_>],
        values: &[Term<'_>],
    ) -> NifResult<Self> {
        Self::from_arrays(env, keys, values)
    }

    /// Construct a map from key-value pairs.
    pub fn from_pairs(env: Env<'a>, pairs: &[(impl Encoder, impl Encoder)]) -> NifResult<Self> {
        let (keys, values): (Vec<_>, Vec<_>) = pairs
            .iter()
            .map(|(key, value)| (key.encode(env).as_c_arg(), value.encode(env).as_c_arg()))
            .unzip();
        Self::from_raw_arrays(env, &keys, &values)
    }

    /// Get a value, returning `BadArg` if the key is absent.
    pub fn get<'b>(self, env: Env<'b>, key: impl Encoder) -> NifResult<Term<'b>> {
        let map = self.encode(env);
        let mut result = MaybeUninit::uninit();
        let success = unsafe {
            enif_get_map_value(
                env.as_c_arg(),
                map.as_c_arg(),
                key.encode(env).as_c_arg(),
                result.as_mut_ptr(),
            )
        };
        if success != 1 {
            return Err(Error::BadArg);
        }
        Ok(unsafe { Term::new(env, result.assume_init()) })
    }

    /// Get the number of entries.
    pub fn size(self, env: Env<'_>) -> NifResult<usize> {
        let map = self.encode(env);
        let mut size = MaybeUninit::uninit();
        let success =
            unsafe { enif_get_map_size(env.as_c_arg(), map.as_c_arg(), size.as_mut_ptr()) };
        if success != 1 {
            return Err(Error::BadArg);
        }
        Ok(unsafe { size.assume_init() })
    }

    /// Return a map with `key` set to `value`, replacing any existing entry.
    pub fn put<'b>(
        self,
        env: Env<'b>,
        key: impl Encoder,
        value: impl Encoder,
    ) -> NifResult<Map<'b>> {
        let map = self.encode(env);
        let mut result = MaybeUninit::uninit();
        let success = unsafe {
            enif_make_map_put(
                env.as_c_arg(),
                map.as_c_arg(),
                key.encode(env).as_c_arg(),
                value.encode(env).as_c_arg(),
                result.as_mut_ptr(),
            )
        };
        if success != 1 {
            return Err(Error::BadArg);
        }
        Ok(unsafe { Map::from_raw(result.assume_init(), env) })
    }

    /// Return a map without `key`. An absent key leaves the map unchanged.
    pub fn remove<'b>(self, env: Env<'b>, key: impl Encoder) -> NifResult<Map<'b>> {
        let map = self.encode(env);
        let mut result = MaybeUninit::uninit();
        let success = unsafe {
            enif_make_map_remove(
                env.as_c_arg(),
                map.as_c_arg(),
                key.encode(env).as_c_arg(),
                result.as_mut_ptr(),
            )
        };
        if success != 1 {
            return Err(Error::BadArg);
        }
        Ok(unsafe { Map::from_raw(result.assume_init(), env) })
    }

    /// Replace an existing value, returning `BadArg` if the key is absent.
    pub fn update<'b>(
        self,
        env: Env<'b>,
        key: impl Encoder,
        value: impl Encoder,
    ) -> NifResult<Map<'b>> {
        let map = self.encode(env);
        let mut result = MaybeUninit::uninit();
        let success = unsafe {
            enif_make_map_update(
                env.as_c_arg(),
                map.as_c_arg(),
                key.encode(env).as_c_arg(),
                value.encode(env).as_c_arg(),
                result.as_mut_ptr(),
            )
        };
        if success != 1 {
            return Err(Error::BadArg);
        }
        Ok(unsafe { Map::from_raw(result.assume_init(), env) })
    }

    /// Iterate over this map without checking its type again.
    pub fn into_iterator<'b>(self, env: Env<'b>) -> MapIterator<'b> {
        unsafe { MapIterator::new_unchecked(self.encode(env)) }
    }

    fn from_raw_arrays(
        env: Env<'a>,
        keys: &[ErlNifTerm],
        values: &[ErlNifTerm],
    ) -> NifResult<Self> {
        unsafe {
            make_map_from_arrays(env.as_c_arg(), keys, values)
                .map(|raw| Self::from_raw(raw, env))
                .ok_or(Error::BadArg)
        }
    }
}

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
