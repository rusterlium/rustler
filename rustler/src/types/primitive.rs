use crate::types::atom;
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};

macro_rules! erl_make {
    ($self:expr, $env:ident, $encode_fun:ident, $type:ty) => {
        #[allow(clippy::cast_lossless)]
        unsafe {
            Term::new(
                $env,
                crate::sys::$encode_fun($env.as_c_arg(), $self as $type),
            )
        }
    };
}

macro_rules! erl_get {
    ($decode_fun:ident, $term:ident, $dest:ident) => {
        unsafe { crate::sys::$decode_fun($term.get_env().as_c_arg(), $term.as_c_arg(), &mut $dest) }
    };
}

macro_rules! impl_number_encoder {
    ($dec_type:ty, $nif_type:ty, $encode_fun:ident) => {
        impl Encoder for $dec_type {
            #[inline]
            fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
                erl_make!(*self, env, $encode_fun, $nif_type)
            }
        }
    };
}

macro_rules! impl_number_decoder {
    ($dec_type:ty, $nif_type:ty, $decode_fun:ident) => {
        impl<'a> Decoder<'a> for $dec_type {
            #[inline]
            fn decode(term: Term) -> NifResult<$dec_type> {
                #![allow(unused_unsafe)]
                let mut res: $nif_type = Default::default();
                if erl_get!($decode_fun, term, res) == 0 {
                    return Err(Error::BadArg);
                }
                Ok(res as $dec_type)
            }
        }
    };
}

macro_rules! impl_number_transcoder {
    ($dec_type:ty, $nif_type:ty, $encode_fun:ident, $decode_fun:ident) => {
        impl_number_encoder!($dec_type, $nif_type, $encode_fun);
        impl_number_decoder!($dec_type, $nif_type, $decode_fun);
    };
}

// Base number types
impl_number_transcoder!(i32, i32, enif_make_int, enif_get_int);
impl_number_transcoder!(u32, u32, enif_make_uint, enif_get_uint);
impl_number_transcoder!(i64, i64, enif_make_int64, enif_get_int64);
impl_number_transcoder!(u64, u64, enif_make_uint64, enif_get_uint64);
impl_number_encoder!(f64, f64, enif_make_double);

// Casted number types
impl_number_transcoder!(i8, i32, enif_make_int, enif_get_int);
impl_number_transcoder!(u8, u32, enif_make_uint, enif_get_uint);
impl_number_transcoder!(i16, i32, enif_make_int, enif_get_int);
impl_number_transcoder!(u16, u32, enif_make_uint, enif_get_uint);
impl_number_transcoder!(usize, u64, enif_make_uint64, enif_get_uint64);
impl_number_transcoder!(isize, i64, enif_make_int64, enif_get_int64);
impl_number_encoder!(f32, f64, enif_make_double);

// Manual Decoder impls for floats so they can fall back to decoding from integer terms
impl Decoder<'_> for f64 {
    fn decode(term: Term) -> NifResult<f64> {
        #![allow(unused_unsafe)]
        let mut res: f64 = Default::default();
        if erl_get!(enif_get_double, term, res) == 0 {
            let res_fallback: i64 = term.decode()?;
            return Ok(res_fallback as f64);
        }
        Ok(res)
    }
}

impl Decoder<'_> for f32 {
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
