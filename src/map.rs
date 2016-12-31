//! Utilities used to access and create Erlang maps.

use super::{ NifEnv, NifTerm };
use ::wrapper::map;

pub fn map_new<'a>(env: &'a NifEnv) -> NifTerm<'a> {
    NifTerm::new(env, map::map_new(env.as_c_arg()))
}

impl<'a> NifTerm<'a> {

    pub fn map_get(self, key: NifTerm) -> Option<NifTerm<'a>> {
        let env = self.get_env();
        match ::wrapper::get_map_value(env.as_c_arg(), self.as_c_arg(), key.as_c_arg()) {
            Some(value) => Some(NifTerm::new(env, value)),
            None => None,
        }
    }

    pub fn map_size(self) -> Option<usize> {
        let env = self.get_env();
        map::get_map_size(env.as_c_arg(), self.as_c_arg())
    }

    pub fn map_put(self, key: NifTerm, value: NifTerm) -> Option<NifTerm<'a>> {
        let map_env = self.get_env();

        assert!(map_env == key.get_env(), "key is from different environment as map");
        assert!(map_env == value.get_env(), "value is from different environment as map");

        match map::map_put(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg(), value.as_c_arg()) {
            Some(inner) => Some(NifTerm::new(map_env, inner)),
            None => None,
        }
    }

    pub fn map_remove(self, key: NifTerm) -> Option<NifTerm<'a>> {
        let map_env = self.get_env();

        assert!(map_env == key.get_env(), "key is from different environment as map");

        match map::map_remove(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg()) {
            Some(inner) => Some(NifTerm::new(map_env, inner)),
            None => None,
        }
    }

    pub fn map_update(self, key: NifTerm, new_value: NifTerm) -> Option<NifTerm<'a>> {
        let map_env = self.get_env();

        assert!(map_env == key.get_env(), "key is from different environment as map");
        assert!(map_env == new_value.get_env(), "value is from different environment as map");

        match map::map_update(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg(), new_value.as_c_arg()) {
            Some(inner) => Some(NifTerm::new(map_env, inner)),
            None => None,
        }
    }

}
