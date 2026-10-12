//! List terms, including empty and improper lists.

use crate::sys::{
    enif_get_list_cell, enif_get_list_length, enif_make_list_cell, enif_make_list_from_array,
    enif_make_reverse_list, ErlNifEnv, ErlNifTerm,
};
use crate::{BoundTerm, Encoder, Env, Error, ListIterator, NifResult, Term};
use std::mem::MaybeUninit;

bound_term!(List, |term: crate::Term<'_>| {
    term.is_list() || term.is_empty_list()
});

impl<'a> List<'a> {
    /// Construct an empty list.
    pub fn new_empty(env: Env<'a>) -> Self {
        unsafe { Self::from_raw(make_list(env.as_c_arg(), &[]), env) }
    }

    /// Iterate without checking the list's type again.
    ///
    /// As with `ListIterator`, reaching an improper tail panics.
    pub fn into_iterator<'b>(self, env: Env<'b>) -> ListIterator<'b> {
        unsafe { ListIterator::new_unchecked(self.encode(env)) }
    }

    /// Get the length of a proper list. Improper lists return `BadArg`.
    pub fn length(self, env: Env<'_>) -> NifResult<usize> {
        let list = self.encode(env);
        let mut length = 0;
        let success = unsafe { enif_get_list_length(env.as_c_arg(), list.as_c_arg(), &mut length) };
        if success != 1 {
            return Err(Error::BadArg);
        }
        Ok(length as usize)
    }

    /// Get the head and tail, returning `BadArg` for an empty list.
    ///
    /// The tail may be any term when this is an improper list.
    pub fn get_cell<'b>(self, env: Env<'b>) -> NifResult<(Term<'b>, Term<'b>)> {
        let list = self.encode(env);
        unsafe {
            get_list_cell(env.as_c_arg(), list.as_c_arg())
                .map(|(head, tail)| (Term::new(env, head), Term::new(env, tail)))
                .ok_or(Error::BadArg)
        }
    }

    /// Reverse a proper list. Improper lists return `BadArg`.
    pub fn reverse<'b>(self, env: Env<'b>) -> NifResult<List<'b>> {
        let list = self.encode(env);
        let mut result = MaybeUninit::uninit();
        let success =
            unsafe { enif_make_reverse_list(env.as_c_arg(), list.as_c_arg(), result.as_mut_ptr()) };
        if success != 1 {
            return Err(Error::BadArg);
        }
        Ok(unsafe { List::from_raw(result.assume_init(), env) })
    }

    /// Add a head while preserving this list as the tail.
    pub fn prepend<'b>(self, env: Env<'b>, head: impl Encoder) -> List<'b> {
        unsafe {
            List::from_raw(
                make_list_cell(
                    env.as_c_arg(),
                    head.encode(env).as_c_arg(),
                    self.encode(env).as_c_arg(),
                ),
                env,
            )
        }
    }
}

pub(crate) unsafe fn get_list_cell(
    env: *mut ErlNifEnv,
    list: ErlNifTerm,
) -> Option<(ErlNifTerm, ErlNifTerm)> {
    let mut head = MaybeUninit::uninit();
    let mut tail = MaybeUninit::uninit();
    if enif_get_list_cell(env, list, head.as_mut_ptr(), tail.as_mut_ptr()) != 1 {
        return None;
    }
    Some((head.assume_init(), tail.assume_init()))
}

pub(crate) unsafe fn make_list(env: *mut ErlNifEnv, arr: &[ErlNifTerm]) -> ErlNifTerm {
    enif_make_list_from_array(env, arr.as_ptr(), arr.len() as u32)
}

pub(crate) unsafe fn make_list_cell(
    env: *mut ErlNifEnv,
    head: ErlNifTerm,
    tail: ErlNifTerm,
) -> ErlNifTerm {
    enif_make_list_cell(env, head, tail)
}
