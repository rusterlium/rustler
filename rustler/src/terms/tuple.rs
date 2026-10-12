//! Tuple terms.

use crate::sys::{c_int, enif_get_tuple, enif_make_tuple_from_array, ErlNifEnv, ErlNifTerm};
use crate::{BoundTerm, Encoder, Env, Error, NifResult, Term};
use std::mem::MaybeUninit;

bound_term!(Tuple, crate::Term::is_tuple);

impl<'a> Tuple<'a> {
    /// Construct a tuple, copying its elements into `env` when needed.
    pub fn make_tuple(env: Env<'a>, terms: &[Term<'_>]) -> Self {
        let terms: Vec<_> = terms
            .iter()
            .map(|term| term.in_env(env).as_c_arg())
            .collect();
        unsafe { Self::from_raw(make_tuple_raw(env.as_c_arg(), &terms), env) }
    }

    /// Get the tuple's elements.
    pub fn get_tuple<'b>(self, env: Env<'b>) -> NifResult<Vec<Term<'b>>> {
        let tuple = self.encode(env);
        unsafe {
            get_tuple_raw(env.as_c_arg(), tuple.as_c_arg())
                .map(|terms| terms.iter().map(|raw| Term::new(env, *raw)).collect())
        }
    }
}

pub(crate) unsafe fn get_tuple_raw<'a>(
    env: *mut ErlNifEnv,
    term: ErlNifTerm,
) -> Result<&'a [ErlNifTerm], Error> {
    let mut arity: c_int = 0;
    let mut array_ptr = MaybeUninit::uninit();
    if enif_get_tuple(env, term, &mut arity, array_ptr.as_mut_ptr()) != 1 {
        return Err(Error::BadArg);
    }
    Ok(std::slice::from_raw_parts(
        array_ptr.assume_init(),
        arity as usize,
    ))
}

pub(crate) unsafe fn make_tuple_raw(env: *mut ErlNifEnv, terms: &[ErlNifTerm]) -> ErlNifTerm {
    enif_make_tuple_from_array(env, terms.as_ptr(), terms.len() as u32)
}
