use super::wrapper;

use std::mem::MaybeUninit;

use crate::sys::{
    enif_get_list_cell, enif_get_list_length, enif_make_list_cell, enif_make_reverse_list,
    get_enif_make_list,
};
use crate::wrapper::{list, NIF_TERM};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term, TermType};

wrapper!(
    /// Enables iteration over the items in the list.
    ///
    /// Although this behaves like a standard Rust iterator
    /// ([book](https://doc.rust-lang.org/book/iterators.html) /
    /// [docs](https://doc.rust-lang.org/std/iter/trait.Iterator.html)), there are a couple of tricky
    /// parts to using it.
    ///
    /// Because the iterator is an iterator over `Term`s, you need to decode the terms before you
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
    /// # use rustler::{Term, NifResult};
    /// # use rustler::types::list::ListIterator;
    /// # fn list_iterator_example(list_term: Term) -> NifResult<Vec<i64>> {
    /// let list_iterator: ListIterator = list_term.decode()?;
    ///
    /// let result: NifResult<Vec<i64>> = list_iterator
    ///     // Produces an iterator of NifResult<i64>
    ///     .map(|x| x.decode::<i64>())
    ///     // Lifts each value out of the result. Returns Ok(Vec<i64>) if successful, the first error
    ///     // Error(Error) on failure.
    ///     .collect::<NifResult<Vec<i64>>>();
    /// # result
    /// # }
    /// ```
    struct ListIterator(TermType::List)
);

impl<'a> Iterator for ListIterator<'a> {
    type Item = Term<'a>;

    fn next(&mut self) -> Option<Term<'a>> {
        match self.get_cell() {
            Some((head, tail)) => {
                // TODO: This is unsafe as tail might not be a list.
                self.0 = tail;
                Some(head)
            },
            None => {
                if self.is_empty_list() {
                    // We reached the end of the list, finish the iterator.
                    None
                } else {
                    panic!("list iterator found improper list")
                }
            }
        }
    }
}

//impl<'a, T> Encoder for Iterator<Item = T> where T: Encoder {
//    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
//        let term_arr: Vec<NIF_TERM> =
//            self.map(|x| x.encode(env).as_c_arg()).collect();
//    }
//}
impl<T> Encoder for Vec<T>
where
    T: Encoder,
{
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        self.as_slice().encode(env)
    }
}

impl<'a, T> Decoder<'a> for Vec<T>
where
    T: Decoder<'a>,
{
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let iter: ListIterator = term.decode()?;
        let res: NifResult<Self> = iter.map(|x| x.decode::<T>()).collect();
        res
    }
}

impl<T> Encoder for [T]
where
    T: Encoder,
{
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let term_array: Vec<NIF_TERM> = self.iter().map(|x| x.encode(env).as_c_arg()).collect();
        unsafe { Term::new(env, list::make_list(env.as_c_arg(), &term_array)) }
    }
}

impl<'a, T> Encoder for &'a [T]
where
    T: Encoder,
{
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
        let term_array: Vec<NIF_TERM> = self.iter().map(|x| x.encode(env).as_c_arg()).collect();
        unsafe { Term::new(env, list::make_list(env.as_c_arg(), &term_array)) }
    }
}

impl<'a> ListIterator<'a> {
    pub fn new_empty(env: Env<'a>) -> Self {
        let term = unsafe { get_enif_make_list()(env.as_c_arg(), 0) };
        unsafe { Self::wrap_unchecked(Term::new(env, term)) }
    }

    pub fn len(&self) -> Option<usize> {
        let mut len: u32 = 0;
        let success =
            unsafe { enif_get_list_length(self.get_env().as_c_arg(), self.as_c_arg(), &mut len) };

        if success != 1 {
            return None;
        }

        Some(len as usize)
    }

    pub fn empty(&self) -> bool {
        self.is_empty_list()
    }

    pub fn get_cell(&self) -> Option<(Term<'a>, Term<'a>)> {
        let env = self.get_env();
        let mut head = MaybeUninit::uninit();
        let mut tail = MaybeUninit::uninit();
        let success = unsafe {
            enif_get_list_cell(
                env.as_c_arg(),
                self.as_c_arg(),
                head.as_mut_ptr(),
                tail.as_mut_ptr(),
            )
        };

        if success != 1 {
            return None;
        }

        unsafe {
            Some((
                Term::new(env, head.assume_init()),
                Term::new(env, tail.assume_init()),
            ))
        }
    }

    pub fn prepend(&self, head: impl Encoder) -> Self {
        Term::list_prepend(self.unwrap(), head)
    }

    pub fn reverse(&self) -> Option<Self> {
        let env = self.get_env();
        let mut list_out = MaybeUninit::uninit();
        let success = unsafe {
            enif_make_reverse_list(env.as_c_arg(), self.as_c_arg(), list_out.as_mut_ptr())
        };

        if success != 1 {
            return None;
        }

        let term = unsafe { Self::wrap_unchecked(Term::new(env, list_out.assume_init())) };

        Some(term)
    }
}

/// ## List terms
impl<'a> Term<'a> {
    /// Returns a new empty list.
    pub fn list_new_empty(env: Env<'a>) -> ListIterator<'a> {
        ListIterator::new_empty(env)
    }

    /// Returns an iterator over a list term.
    /// See documentation for ListIterator for more information.
    ///
    /// Returns None if the term is not a list.
    pub fn into_list_iterator(self) -> NifResult<ListIterator<'a>> {
        Ok(ListIterator::wrap(self)?)
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
        let iter: ListIterator = self.try_into()?;
        iter.len().ok_or(Error::BadArg)
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
    pub fn list_get_cell(self) -> NifResult<Option<(Term<'a>, Term<'a>)>> {
        let iter: ListIterator = self.try_into()?;

        Ok(iter.get_cell())
    }

    /// Makes a copy of the self list term and reverses it.
    ///
    /// Returns Err(Error::BadArg) if the term is not a list.
    pub fn list_reverse(self) -> NifResult<ListIterator<'a>> {
        let iter: ListIterator = self.try_into()?;
        iter.reverse().ok_or(Error::BadArg)
    }

    /// Adds `head` in a list cell with `self` as tail.
    pub fn list_prepend(self, head: impl Encoder) -> ListIterator<'a> {
        let env = self.get_env();
        let head = head.encode(env);

        unsafe {
            let term = enif_make_list_cell(env.as_c_arg(), head.as_c_arg(), self.as_c_arg());
            ListIterator::wrap_unchecked(Term::new(env, term))
        }
    }
}
