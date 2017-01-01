//! Utilities used for working with erlang linked lists.
//!
//! Right now the only supported way to read lists are through the NifListIterator.

use super::{ NifTerm, NifError, NifResult, NifDecoder, NifEncoder, NifEnv };
use ::wrapper::list;
use ::wrapper::check;

/// Enables iteration over the items in the list.
///
/// Although this behaves like a standard Rust iterator
/// ([book](https://doc.rust-lang.org/book/iterators.html) /
/// [docs](https://doc.rust-lang.org/std/iter/trait.Iterator.html)), there are a couple of tricky
/// parts to using it.
///
/// Because the iterator is an iterator over `NifTerm`s, you need to decode the terms before you
/// can do anything with them.
///
/// ## Example
/// An easy way to decode all terms in a list, is to use the `.map()` function of the iterator, and
/// decode every entry in the list. This will produce an iterator of `Result`s, and will therefore
/// not be directly usable in the way you might immediately expect.
///
/// For this case, the the `.collect()` function of rust iterators is useful, as it can lift
/// the `Result`s out of the list. (Contains extra type annotations for clarity)
///
/// ```
/// let list_iterator: NifListIterator = try!(list_term.decode());
///
/// let result: NifResult<Vec<i64>> = list_interator
///     // Produces an iterator of NifResult<i64>
///     .map(|x| x.decode::<i64>())
///     // Lifts each value out of the result. Returns Ok(Vec<i64>) if successful, the first error
///     // Error(NifError) on failure.
///     .collect::<NifResult<Vec<i64>>>();
/// ```
pub struct NifListIterator<'a> {
    term: NifTerm<'a>,
}

impl<'a> NifListIterator<'a> {

    fn new(term: NifTerm<'a>) -> Option<Self> {
        let term_c = term.as_c_arg();
        let env_c = term.get_env().as_c_arg();
        if check::is_list(env_c, term_c) || check::is_empty_list(env_c, term_c) {
            let iter = NifListIterator {
                term: term,
            };
            Some(iter)
        } else {
            None
        }
    }

}

impl<'a> Iterator for NifListIterator<'a> {
    type Item = NifTerm<'a>;

    fn next(&mut self) -> Option<NifTerm<'a>> {
        let env = self.term.get_env();
        let cell = list::get_list_cell(env.as_c_arg(), self.term.as_c_arg());

        match cell {
            Some((head, tail)) => {
                self.term = NifTerm::new(self.term.get_env(), tail);
                Some(NifTerm::new(self.term.get_env(), head))
            }
            None => {
                if check::is_empty_list(env.as_c_arg(), self.term.as_c_arg()) {
                    // We reached the end of the list, finish the iterator.
                    None
                } else {
                    panic!("list iterator found improper list")
                }
            }
        }
    }
}

impl<'a> NifDecoder<'a> for NifListIterator<'a> {
    fn decode(term: NifTerm<'a>) -> NifResult<Self> {
        match NifListIterator::new(term) {
            Some(iter) => Ok(iter),
            None => Err(NifError::BadArg)
        }
    }
}

//impl<'a, T> NifEncoder for Iterator<Item = T> where T: NifEncoder {
//    fn encode<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
//        let term_arr: Vec<::wrapper::nif_interface::NIF_TERM> =
//            self.map(|x| x.encode(env).as_c_arg()).collect();
//    }
//}

impl<'a, T> NifEncoder for Vec<T> where T: NifEncoder {
    fn encode<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        let term_array: Vec<::wrapper::nif_interface::NIF_TERM> =
            self.iter().map(|x| x.encode(env).as_c_arg()).collect();
        NifTerm::new(env, list::make_list(env.as_c_arg(), &term_array))
    }
}

impl<'a, T> NifDecoder<'a> for Vec<T> where T: NifDecoder<'a> {
    fn decode(term: NifTerm<'a>) -> NifResult<Self> {
        let iter: NifListIterator = try!(term.decode());
        let res: NifResult<Self> = iter
            .map(|x| x.decode::<T>())
            .collect();
        res
    }
}

/// ## List terms
impl<'a> NifTerm<'a> {

    /// Returns an iterator over a list term.
    /// See documentation for NifListIterator for more information.
    ///
    /// Returns None if the term is not a list.
    pub fn into_list_iterator(self) -> NifResult<NifListIterator<'a>> {
        NifListIterator::new(self).ok_or(NifError::BadArg)
    }

    /// Returns the length of a list term.
    ///
    /// Returns None if the term is not a list.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// length(self_term)
    /// ```
    pub fn list_length(self) -> NifResult<usize> {
        list::get_list_length(self.get_env().as_c_arg(), self.as_c_arg())
            .ok_or(NifError::BadArg)
    }

    /// Unpacks a single cell at the head of a list term,
    /// and returns the result as a tuple of (head, tail).
    ///
    /// Returns None if the term is not a list.
    ///
    /// ### Elixir equivalent
    /// ```elixir
    /// [head, tail] = self_term
    /// {head, tail}
    /// ```
    pub fn list_get_cell(self) -> NifResult<(NifTerm<'a>, NifTerm<'a>)> {
        let env = self.get_env();
        list::get_list_cell(env.as_c_arg(), self.as_c_arg())
            .map(|(t1, t2)| (NifTerm::new(env, t1), NifTerm::new(env, t2)))
            .ok_or(NifError::BadArg)
    }

    /// Makes a copy of the self list term and reverses it.
    ///
    /// Returns Err(NifError::BadArg) if the term is not a list.
    pub fn list_reverse(self) -> NifResult<NifTerm<'a>> {
        let env = self.get_env();
        list::make_reverse_list(env.as_c_arg(), self.as_c_arg())
            .map(|t| NifTerm::new(env, t))
            .ok_or(NifError::BadArg)
    }

}
