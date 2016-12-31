use ::{NifDecoder, NifEnv, NifResult};
use ::wrapper::nif_interface::{NIF_TERM};

/// NifTerm is used to represent all erlang terms. Terms are always lifetime limited by a NifEnv.
///
/// NifTerm is cloneable and copyable, but it can not exist outside of the lifetime of the NifEnv
/// that owns it.
#[derive(Clone, Copy)]
pub struct NifTerm<'a> {
    term: NIF_TERM,
    env: &'a NifEnv,
}

impl<'a> NifTerm<'a> {
    pub fn new(env: &'a NifEnv, inner: NIF_TERM) -> Self {
        NifTerm {
            term: inner,
            env: env,
        }
    }
    /// This extracts the raw term pointer. It is usually used in order to obtain a type that can
    /// be passed to calls into the erlang vm.
    pub fn as_c_arg(&self) -> NIF_TERM {
        self.term
    }

    pub fn get_env(&self) -> &'a NifEnv {
        self.env
    }

    /// This will coerce the NifTerm into the given NifEnv without providing any checks
    /// or other operations. This is unsafe as it allows you to make a NifTerm usable
    /// on an env other then the one that owns it.
    /// 
    /// The one case where this is acceptable to use is when we already know the NifEnv
    /// is the same, but we need to coerce the lifetime.
    pub unsafe fn env_cast<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        NifTerm::new(env, self.as_c_arg())
    }

    /// Returns a representation of self in the given NifEnv.
    ///
    /// If the term is already is in the provided env, it will be directly returned. Otherwise
    /// the term will be copied over.
    pub fn in_env<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        if self.get_env() == env {
            unsafe { self.env_cast(env) }
        } else {
            NifTerm::new(env, ::wrapper::copy_term(env.as_c_arg(), self.as_c_arg()))
        }
    }

    pub fn get_type(&self) -> ::dynamic::TermType {
        ::dynamic::get_type(self)
    }

    /// Decodes the NifTerm into type T.
    /// 
    /// This should be used as the primary method of extracting the value from a NifTerm.
    /// 
    /// # Examples
    ///
    /// ```ignore
    /// let term: NifTerm = ...;
    /// let number: i32 = try!(term.decode());
    /// ```
    pub fn decode<T>(self) -> NifResult<T> where T: NifDecoder<'a> {
        NifDecoder::decode(self)
    }
}
