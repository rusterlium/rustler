use crate::sys::{enif_get_tuple, enif_make_tuple_from_array, ERL_NIF_TERM};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term, TermType};

use std::ffi::c_int;
use std::mem::MaybeUninit;

use super::wrapper;
wrapper!(
    struct Tuple(TermType::Tuple)
);

pub unsafe fn get_tuple(term: Term<'_>) -> NifResult<&[ERL_NIF_TERM]> {
    let env = term.get_env();
    let mut arity: c_int = 0;
    let mut array_ptr = MaybeUninit::uninit();
    let success = enif_get_tuple(
        env.as_c_arg(),
        term.as_c_arg(),
        &mut arity,
        array_ptr.as_mut_ptr(),
    );
    if success != 1 {
        return Err(Error::BadArg);
    }
    let term_array = ::std::slice::from_raw_parts(array_ptr.assume_init(), arity as usize);
    Ok(term_array)
}

impl<'a> Tuple<'a> {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.get_elements().len()
    }

    pub fn get(&self, index: usize) -> Option<Term<'a>> {
        self.get_elements()
            .get(index)
            .map(|ptr| unsafe { Term::new(self.get_env(), *ptr) })
    }

    /// Convert an Erlang tuple to a Rust vector. (To convert to a Rust tuple, use `term.decode()`
    /// instead.)
    ///
    /// # Errors
    /// `badarg` if `term` is not a tuple.
    pub fn to_vec(&self) -> Vec<Term<'a>> {
        self.get_elements()
            .iter()
            .map(|ptr| unsafe { Term::new(self.get_env(), *ptr) })
            .collect()
    }

    fn get_elements(&self) -> &'a [ERL_NIF_TERM] {
        unsafe { get_tuple(self.unwrap()).unwrap() }
    }
}

impl<'a> Env<'a> {
    pub fn make_tuple(&self, terms: &[Term<'a>]) -> Tuple<'a> {
        make_tuple(*self, terms)
    }
}

/// Convert a vector of terms to an Erlang tuple. (To convert from a Rust tuple to an Erlang tuple,
/// use `Encoder` instead.)
pub fn make_tuple<'a>(env: Env<'a>, terms: &[Term]) -> Tuple<'a> {
    let c_terms: Vec<ERL_NIF_TERM> = terms.iter().map(|term| term.as_c_arg()).collect();
    unsafe {
        let term =
            enif_make_tuple_from_array(env.as_c_arg(), c_terms.as_ptr(), c_terms.len() as u32);
        Term::new(env, term).try_into().unwrap()
    }
}

/// Helper macro to emit tuple-like syntax. Wraps its arguments in parentheses, and adds a comma if
/// there's exactly one argument.
macro_rules! tuple {
    ( ) => { () };
    ( $e0:tt ) => { ($e0,) };
    ( $( $e:tt ),* ) => { ( $( $e ),* ) };
}

/// Helper macro that returns the number of comma-separated expressions passed to it.
/// For example, `count!(a + b, c)` evaluates to `2`.
macro_rules! count {
    ( ) => ( 0 );
    ( $blah:expr ) => ( 1 );
    ( $blah:expr, $( $others:expr ),* ) => ( 1 + count!( $( $others ),* ) )
}

macro_rules! impl_nifencoder_nifdecoder_for_tuple {
    ( $($index:tt : $tyvar:ident),* ) => {
        // No need for `$crate` gunk in here, since the macro is not exported.
        impl<$( $tyvar: Encoder ),*>
            Encoder for tuple!( $( $tyvar ),* )
        {
            fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
                let arr = [ $( Encoder::encode(&self.$index, env) ),* ];
                make_tuple(env, &arr).into()
            }
        }

        impl<'a, $( $tyvar: Decoder<'a> ),*>
            Decoder<'a> for tuple!( $( $tyvar ),* )
        {
            fn decode(term: Term<'a>) -> NifResult<tuple!( $( $tyvar ),* )>
            {
                match unsafe { get_tuple(term) } {
                    Ok(elements) if elements.len() == count!( $( $index ),* ) =>
                        Ok(tuple!( $(
                            (<$tyvar as Decoder>::decode(
                                unsafe { Term::new(term.get_env(), elements[$index]) })?)
                        ),* )),
                    _ =>
                        Err(Error::BadArg),
                }
            }
        }
    }
}

impl_nifencoder_nifdecoder_for_tuple!();
impl_nifencoder_nifdecoder_for_tuple!(0: A);
impl_nifencoder_nifdecoder_for_tuple!(0: A, 1: B);
impl_nifencoder_nifdecoder_for_tuple!(0: A, 1: B, 2: C);
impl_nifencoder_nifdecoder_for_tuple!(0: A, 1: B, 2: C, 3: D);
impl_nifencoder_nifdecoder_for_tuple!(0: A, 1: B, 2: C, 3: D, 4: E);
impl_nifencoder_nifdecoder_for_tuple!(0: A, 1: B, 2: C, 3: D, 4: E, 5: F);
impl_nifencoder_nifdecoder_for_tuple!(0: A, 1: B, 2: C, 3: D, 4: E, 5: F, 6: G);
