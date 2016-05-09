//! Utilities used for working with erlang linked lists.
//!
//! Right now the only supported way to read lists are through the NifListIterator.

use super::{ NifTerm, NifError, NifResult, NifDecoder };
use ::wrapper::list;

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
