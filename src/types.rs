extern crate ruster_unsafe;
extern crate libc;

use super::{ NifEnv, NifTerm, NifError };

pub trait NifEncoder {
    fn encode<'a>(&self, env: &'a NifEnv) -> NifTerm<'a>;
}
pub trait NifDecoder<'a>: Sized {
    fn decode(term: NifTerm, env: &'a NifEnv) -> Result<Self, NifError>;
}

macro_rules! impl_number_transcoder {
    ($typ:ty, $encode_fun:ident, $decode_fun:ident) => {
        impl NifEncoder for $typ {
            fn encode<'a>(&self, env: &'a NifEnv) -> NifTerm<'a> {
                #![allow(unused_unsafe)]
                NifTerm::new(env, unsafe { ruster_unsafe::$encode_fun(env.env, self.clone()) })
            }
        }
        impl<'a> NifDecoder<'a> for $typ {
            fn decode(term: NifTerm, env: &'a NifEnv) -> Result<$typ, NifError> {
                #![allow(unused_unsafe)]
                let mut res: $typ = Default::default();
                if unsafe { ruster_unsafe::$decode_fun(env.env, term.term, (&mut res) as *mut $typ) } == 0 {
                    return Err(NifError::BadArg);
                }
                Ok(res)
            }
        }
    }
}

impl_number_transcoder!(libc::c_int, enif_make_int, enif_get_int);
impl_number_transcoder!(libc::c_uint, enif_make_uint, enif_get_uint);
impl_number_transcoder!(u64, enif_make_uint64, enif_get_uint64);
impl_number_transcoder!(i64, enif_make_int64, enif_get_int64);
impl_number_transcoder!(libc::c_double, enif_make_double, enif_get_double);

use super::atom::{ get_atom };
impl NifEncoder for bool {
    fn encode<'a>(&self, env: &'a NifEnv) -> NifTerm<'a> {
        if self.clone() {
            get_atom("true").unwrap().to_term(env)
        } else {
            get_atom("false").unwrap().to_term(env)
        }
    }
}
impl<'a> NifDecoder<'a> for bool {
    fn decode(term: NifTerm, env: &'a NifEnv) -> Result<bool, NifError> {
        Ok(super::atom::is_term_truthy(term, env))
    }
}

impl<'a> NifDecoder<'a> for String {
    fn decode(term: NifTerm, env: &NifEnv) -> Result<Self, NifError> {
        let binary = try!(::binary::get_binary(env, term));
        let string = match ::std::str::from_utf8(binary.as_slice()) {
            Ok(string) => string,
            Err(_) => return Err(NifError::BadArg),
        };
        Ok(string.to_string())
    }
}
impl<'a> NifDecoder<'a> for &'a str {
    fn decode(term: NifTerm, env: &'a NifEnv) -> Result<&'a str, NifError> {
        let binary = try!(::binary::get_binary(env, term));
        match ::std::str::from_utf8(binary.as_slice()) {
            Ok(string) => Ok(string),
            Err(_) => return Err(NifError::BadArg),
        }
    }
}

use std::io::Write;

impl NifEncoder for str {
    fn encode<'b>(&self, env: &'b NifEnv) -> NifTerm<'b> {
        let str_len = self.len();
        let mut binary_alloc = match ::binary::alloc_binary(env, str_len) {
            Ok(bin) => bin,
            Err(_) => panic!("binary term alloc failed")
        };
        binary_alloc.as_mut_slice().write(self.as_bytes()).expect("memory copy of string failed");
        let binary_term = ::binary::make_binary(env, &mut binary_alloc);
        binary_term
    }
}
