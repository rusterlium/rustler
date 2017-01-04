//! Utilities used to access and create Erlang maps.

use ::{ NifEnv, NifTerm, NifResult, NifError };
use ::wrapper::map;

pub fn map_new<'a>(env: &'a NifEnv) -> NifTerm<'a> {
    NifTerm::new(env, map::map_new(env.as_c_arg()))
}

/// ## Map terms
impl<'a> NifTerm<'a> {

    /// Gets the value corresponding to a key in a map term.
    ///
    /// Returns Err(NifError::BadArg) if the term is not a map or if
    /// key doesn't exist in the map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// Map.get(self_term, key)
    /// ```
    pub fn map_get(self, key: NifTerm) -> NifResult<NifTerm<'a>> {
        let env = self.get_env();
        match ::wrapper::get_map_value(env.as_c_arg(), self.as_c_arg(), key.as_c_arg()) {
            Some(value) => Ok(NifTerm::new(env, value)),
            None => Err(NifError::BadArg),
        }
    }

    /// Gets the size of a map term.
    ///
    /// Returns Err(NifError::BadArg) if the term is not a map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// map_size(self_term)
    /// ```
    pub fn map_size(self) -> NifResult<usize> {
        let env = self.get_env();
        map::get_map_size(env.as_c_arg(), self.as_c_arg()).ok_or(NifError::BadArg)
    }

    /// Makes a copy of the self map term and sets key to value.
    /// If the value already exists, it is overwritten.
    ///
    /// Returns Err(NifError::BadArg) if the term is not a map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// Map.put(self_term, key, value)
    /// ```
    pub fn map_put(self, key: NifTerm, value: NifTerm) -> NifResult<NifTerm<'a>> {
        let map_env = self.get_env();

        assert!(map_env == key.get_env(), "key is from different environment as map");
        assert!(map_env == value.get_env(), "value is from different environment as map");

        match map::map_put(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg(), value.as_c_arg()) {
            Some(inner) => Ok(NifTerm::new(map_env, inner)),
            None => Err(NifError::BadArg),
        }
    }

    /// Makes a copy of the self map term and removes key. If the key
    /// doesn't exist, the original map is returned.
    ///
    /// Returns Err(NifError::BadArg) if the term is not a map.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// Map.delete(self_term, key)
    /// ```
    pub fn map_remove(self, key: NifTerm) -> NifResult<NifTerm<'a>> {
        let map_env = self.get_env();

        assert!(map_env == key.get_env(), "key is from different environment as map");

        match map::map_remove(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg()) {
            Some(inner) => Ok(NifTerm::new(map_env, inner)),
            None => Err(NifError::BadArg),
        }
    }

    /// Makes a copy of the self map term where key is set to value.
    ///
    /// Returns Err(NifError::BadArg) if the term is not a map of if key
    /// doesn't exist.
    pub fn map_update(self, key: NifTerm, new_value: NifTerm) -> NifResult<NifTerm<'a>> {
        let map_env = self.get_env();

        assert!(map_env == key.get_env(), "key is from different environment as map");
        assert!(map_env == new_value.get_env(), "value is from different environment as map");

        match map::map_update(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg(), new_value.as_c_arg()) {
            Some(inner) => Ok(NifTerm::new(map_env, inner)),
            None => Err(NifError::BadArg),
        }
    }

}
