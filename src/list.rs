//! Utilities used for working with erlang linked lists.
//!
//! Right now the only supported way to read lists are through the NifListIterator.

use super::{ NifTerm, NifError, NifResult, NifDecoder, NifEncoder, NifEnv };
use ::wrapper::list;

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
/// let result: NifResult<Vec<i64>> = iter
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
        if list::is_list(env_c, term_c) || list::is_empty_list(env_c, term_c) {
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
                if list::is_empty_list(env.as_c_arg(), self.term.as_c_arg()) {
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
