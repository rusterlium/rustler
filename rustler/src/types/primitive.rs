use crate::types::atom;
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};

macro_rules! impl_number_transcoder {
    ($dec_type:ty, $nif_type:ty, $encode_fun:ident, $decode_fun:ident) => {
        impl Encoder for $dec_type {
            fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
                #[allow(clippy::cast_lossless)]
                unsafe {
                    Term::new(
                        env,
                        rustler_sys::$encode_fun(env.as_c_arg(), *self as $nif_type),
                    )
                }
            }
        }
        impl<'a> Decoder<'a> for $dec_type {
            fn decode(term: Term) -> NifResult<$dec_type> {
                #![allow(unused_unsafe)]
                let mut res: $nif_type = Default::default();
                if unsafe {
                    rustler_sys::$decode_fun(term.get_env().as_c_arg(), term.as_c_arg(), &mut res)
                } == 0
                {
                    return Err(Error::BadArg);
                }
                Ok(res as $dec_type)
            }
        }
    };
}

// Base number types
impl_number_transcoder!(i32, i32, enif_make_int, enif_get_int);
impl_number_transcoder!(u32, u32, enif_make_uint, enif_get_uint);
impl_number_transcoder!(i64, i64, enif_make_int64, enif_get_int64);
impl_number_transcoder!(u64, u64, enif_make_uint64, enif_get_uint64);
impl_number_transcoder!(f64, f64, enif_make_double, enif_get_double);

// Casted number types
impl_number_transcoder!(i8, i32, enif_make_int, enif_get_int);
impl_number_transcoder!(u8, u32, enif_make_uint, enif_get_uint);
impl_number_transcoder!(i16, i32, enif_make_int, enif_get_int);
impl_number_transcoder!(u16, u32, enif_make_uint, enif_get_uint);
impl_number_transcoder!(usize, u64, enif_make_uint64, enif_get_uint64);
impl_number_transcoder!(isize, i64, enif_make_int64, enif_get_int64);

impl Encoder for bool {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        if *self {
            atom::true_().to_term(env)
        } else {
            atom::false_().to_term(env)
        }
    }
}
impl<'a> Decoder<'a> for bool {
    fn decode(term: Term<'a>) -> NifResult<bool> {
        atom::decode_bool(term)
    }
}

impl Encoder for f32 {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        f64::from(*self).encode(env)
    }
}

impl<'a> Decoder<'a> for f32 {
    fn decode(term: Term) -> NifResult<f32> {
        let res: f64 = term.decode()?;
        let res = res as f32;
        // Values bigger than f32 are coerced as infinity
        if res.is_finite() {
            Ok(res)
        } else {
            Err(Error::BadArg)
        }
    }
}
