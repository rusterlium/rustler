extern crate ruster_unsafe;
extern crate libc;

use super::{ NifEnv, NifTerm, NifError };

pub trait NifEncoder {
    fn encode<'a>(self, env: &'a NifEnv) -> NifTerm<'a>;
}
pub trait NifDecoder {
    fn decode<'a>(term: NifTerm, env: &'a NifEnv) -> Result<Self, NifError>;
}

macro_rules! impl_number_transcoder {
    ($typ:ty, $encode_fun:ident, $decode_fun:ident) => {
        impl NifEncoder for $typ {
            fn encode<'a>(self, env: &'a NifEnv) -> NifTerm<'a> {
                #![allow(unused_unsafe)]
                NifTerm::new(env, unsafe { ruster_unsafe::$encode_fun(env.env, self) })
            }
        }
        impl NifDecoder for $typ {
            fn decode<'a>(term: NifTerm, env: &'a NifEnv) -> Result<$typ, NifError> {
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
//impl_number_encoder!(libc::c_long, enif_make_long);
//impl_number_encoder!(libc::c_ulong, enif_make_ulong);

//impl NifEncoder for bool {
//    fn encode<'a>(self, env: &'a NifEnv) -> NifTerm<'a> {
//        NifTerm::new(env, match self {
//            true => ruster_unsafe::enif_make_atom(env.as_c_arg(), b"true\0" as *const u8),
//            false => ruster_unsafe::enif_make_atom(env.as_c_arg(), b"false\0" as *const u8),
//        })
//    }
//}
//impl NifDecoder for bool {
//    fn decode<'a>(term: NifTerm, env: &'a NifEnv) -> Result<Self, NifError> {
//        
//    }
//}

// Start erlang spesific implementations //

// This is problematic, erlang uses Latin1 while rust uses UTF-8. Everything will work for basic
// ascii characters. God knows what happens if it's not. I hope it's not. Please don't.
//impl<'a> NifEncoder for &'a str {
//    fn encode<'b>(self, env: &'b NifEnv) -> NifTerm<'b> {
//        NifTerm::new(env, unsafe {
//            ruster_unsafe::enif_make_string_len(env.env, 
//                                                self.as_ptr() as *const u8,
//                                                self.len() as size_t,
//                                                ruster_unsafe::ErlNifCharEncoding::ERL_NIF_LATIN1)
//        })
//    }
//}
//impl NifDecoder for String {
//    fn decode<'a>(term: NifTerm, env: &'a NifEnv) -> Result<String, NifError> {
//        let mut length: c_uint = 0;
//        if unsafe { ruster_unsafe::enif_get_list_length(env.env, term.term, &mut length as *mut c_uint) } == 0 {
//            return Err(NifError::BadArg);
//        }
//        let buf: Vec<u8> = Vec::with_capacity(length as usize);
//        ruster_unsafe::enif_get_string(env.env, term.term, buf.as_mut_ptr(), buf.len() as c_uint, 
//                                       ruster_unsafe::ErlNifCharEncoding::ERL_NIF_LATIN1);
//        CString::new(buf)
//    }
//}

// End erlang spesific implementations //
