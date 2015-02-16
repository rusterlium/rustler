
use l1;
use std;
use std::marker::ContravariantLifetime;
use std::mem::transmute;
use std::error;

use libc::c_int;
use libc::c_uint;
use libc::c_double;
use libc::c_long;
use libc::c_ulong;


pub use l1::ErlNifEnv as Environment;

#[derive(Copy)]
#[repr(C)]
pub struct Term<'a> {
	term: l1::ERL_NIF_TERM,
	marker: ContravariantLifetime<'a>,
}

#[derive(Copy)]
pub enum NifError {
	Badarg,
}

impl error::Error for NifError {
	fn description(&self) -> &str {
		match *self {
			NifError::Badarg => "bad argument",
		}
	}
}


// stuct Badarg;

// impl error::Error for Badarg {
// 	fn description(&self) -> &str {
// 		"bad argument"
// 	}
// }

pub trait Encodable {
	fn to_term<'a>(&self, env:&'a Environment) -> Term<'a>;
}

pub trait Decodable {
	fn from_term(env:&Environment, term:Term) -> Result<Self, NifError>;
}


// impl Encodable for c_int {
// 	fn to_term<'a>(&self, env:&'a Environment) -> Term<'a> {
// 		unsafe{ transmute(l1::enif_make_int(transmute(env), *self)) }
// 	}
// }
// impl Decodable for c_int {
// 	fn from_term(env:&Environment, term:Term) -> Result<Self, NifError> {
// 		unsafe {
// 			let mut result: Self = 0;
// 			match l1::enif_get_int(transmute(env), transmute(term), &mut result) {
// 				0 => Err(NifError::Badarg),
// 				_ => Ok(result),
// 			}
// 		}
// 	}
// }


macro_rules! implement_simple_encodable {
	($enctype:ty, $fun:path) => (
		impl Encodable for $enctype {
			fn to_term<'a>(&self, env:&'a Environment) -> Term<'a> {
				unsafe{ transmute($fun(transmute(env), *self)) }
			}
		}
	)
}

macro_rules! implement_simple_decodable{
	($dectype:ty, $fun:path) => (

		impl Decodable for $dectype {
			fn from_term(env:&Environment, term:Term) -> Result<Self, NifError> {
				unsafe {
					let mut result: Self = std::mem::uninitialized();
					match $fun(transmute(env), transmute(term), &mut result) {
						0 => Err(NifError::Badarg),
						_ => Ok(result),
					}
				}
			}
		}
	)
}

macro_rules! implement_simple_transcodable {
	($transtype:ty, $encfun:path, $decfun:path) => (
		implement_simple_encodable!($transtype, $encfun);
		implement_simple_decodable!($transtype, $decfun);
	)
}

// implement_simple_encodable!(c_int, l1::enif_make_int);
// implement_simple_decodable!(c_int, l1::enif_get_int);

implement_simple_transcodable!(c_int, l1::enif_make_int, l1::enif_get_int);
implement_simple_transcodable!(c_uint, l1::enif_make_uint, l1::enif_get_uint);
implement_simple_transcodable!(c_double, l1::enif_make_double, l1::enif_get_double);
implement_simple_transcodable!(i64, l1::enif_make_int64, l1::enif_get_int64);
implement_simple_transcodable!(u64, l1::enif_make_uint64, l1::enif_get_uint64);
// implement_simple_transcodable!(c_long, l1::enif_make_long, l1::enif_get_long);
// implement_simple_transcodable!(c_ulong, l1::enif_make_ulong, l1::enif_get_ulong);



//non-simple transcoders to implement:
// atom
// local_pid
// list, list cell, list length
// resource
// string
// binary, sub binary
// make ref


// #[macro_export]
// macro_rules! wrap_l2_nif {
// 	($wrappee:ident) => (
// 		#[no_mangle]
// 		pub extern "C" fn concat_idents!($wrapee, _wrapped)(env: *mut ErlNifEnv,
// 		                          argc: c_int,
// 		                          args: *const ERL_NIF_TERM) -> ERL_NIF_TERM
// 		{
// 		    unsafe {
// 		         match $wrapee(&*env, std::slice::from_raw_buf(transmute(&args), argc as usize)) {
// 		            Ok(x) => transmute(x),
// 		            _ => enif_make_badarg(env),
// 		         }
// 		    }
// 		}
// 	)
// }

#[macro_export]
macro_rules! wrap_l2_nif {
	($wrapper:ident, $wrappee:ident) => (
		#[no_mangle]
		pub extern "C" fn $wrapper(env: *mut ErlNifEnv,
		                          argc: c_int,
		                          args: *const ERL_NIF_TERM) -> ERL_NIF_TERM
		{
		    unsafe {
		         match $wrappee(&*env, std::slice::from_raw_buf(transmute(&args), argc as usize)) {
		            Ok(x) => transmute(x),
		            _ => enif_make_badarg(env),
		         }
		    }
		}
	)
}