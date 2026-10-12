use crate::terms::tuple::{get_tuple_raw, make_tuple_raw};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};

/// Convert an Erlang tuple to a Rust vector. (To convert to a Rust tuple, use `term.decode()`
/// instead.)
///
/// # Errors
/// `badarg` if `term` is not a tuple.
pub fn get_tuple(term: Term) -> Result<Vec<Term>, Error> {
    crate::terms::Tuple::from_term(term)?.get_tuple(term.get_env())
}

/// Convert a vector of terms to an Erlang tuple. (To convert from a Rust tuple to an Erlang tuple,
/// use `Encoder` instead.)
pub fn make_tuple<'a>(env: Env<'a>, terms: &[Term]) -> Term<'a> {
    unsafe { crate::terms::Tuple::make_tuple(env, terms).to_term(env) }
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
                let arr = [ $( Encoder::encode(&self.$index, env).as_c_arg() ),* ];
                unsafe {
                    Term::new(env, make_tuple_raw(env.as_c_arg(), &arr))
                }
            }
        }

        impl<'a, $( $tyvar: Decoder<'a> ),*>
            Decoder<'a> for tuple!( $( $tyvar ),* )
        {
            fn decode(term: Term<'a>) -> NifResult<tuple!( $( $tyvar ),* )>
            {
                match unsafe { get_tuple_raw(term.get_env().as_c_arg(), term.as_c_arg()) } {
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
