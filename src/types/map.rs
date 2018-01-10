//! Utilities used to access and create Erlang maps.

use ::{ Env, Term, NifResult, Error, Decoder };
use ::wrapper::map;

pub fn map_new<'a>(env: Env<'a>) -> Term<'a> {
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

        assert!(map_env == key.get_env(), "key is from different environment as map");
        assert!(map_env == value.get_env(), "value is from different environment as map");

        match unsafe { map::map_put(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg(), value.as_c_arg()) } {
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

        assert!(map_env == key.get_env(), "key is from different environment as map");

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

        assert!(map_env == key.get_env(), "key is from different environment as map");
        assert!(map_env == new_value.get_env(), "value is from different environment as map");

        match unsafe { map::map_update(map_env.as_c_arg(), self.as_c_arg(), key.as_c_arg(), new_value.as_c_arg()) } {
            Some(inner) => Ok(unsafe { Term::new(map_env, inner) }),
            None => Err(Error::BadArg),
        }
    }

}

pub struct NifMapIterator<'a> {
    env: Env<'a>,
    iter: map::ErlNifMapIterator
}

impl<'a> NifMapIterator<'a> {
    pub fn new(map: Term<'a>) -> Option<NifMapIterator<'a>> {
        let env = map.get_env();
        unsafe {
            map::map_iterator_create(env.as_c_arg(), map.as_c_arg())
        }.map(|iter| NifMapIterator { env: env, iter: iter })
    }
}

impl<'a> Drop for NifMapIterator<'a> {
    fn drop(&mut self) {
        unsafe {
            map::map_iterator_destroy(self.env.as_c_arg(), &mut self.iter);
        }
    }
}

impl<'a> Iterator for NifMapIterator<'a> {
    type Item = (Term<'a>, Term<'a>);

    fn next(&mut self) -> Option<(Term<'a>, Term<'a>)> {
        unsafe {
            map::map_iterator_get_pair(self.env.as_c_arg(), &mut self.iter)
                .map(|(key, value)| {
                    map::map_iterator_next(self.env.as_c_arg(), &mut self.iter);
                    (Term::new(self.env, key),
                     Term::new(self.env, value))
                })
        }
    }
}

impl<'a> Decoder<'a> for NifMapIterator<'a> {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        match NifMapIterator::new(term) {
            Some(iter) => Ok(iter),
            None => Err(Error::BadArg)
        }
    }
}
