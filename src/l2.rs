
use l1;
use std;
use std::mem::transmute;
use std::error;

use libc::c_int;
use libc::c_uint;
use libc::c_double;
use libc::c_long;
use libc::c_ulong;


pub use l1::ErlNifEnv as Environment;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct Term {
	term: l1::ERL_NIF_TERM,
}

#[derive(Debug, Copy, Clone)]
pub enum NifError {
	Badarg,
}

// impl error::Error for NifError {
// 	fn description(&self) -> &str {
// 		match *self {
// 			NifError::Badarg => "bad argument",
// 		}
// 	}
// }


// stuct Badarg;

// impl error::Error for Badarg {
// 	fn description(&self) -> &str {
// 		"bad argument"
// 	}
// }

pub trait Encodable {
	fn to_term(&self, env:& Environment) -> Term;
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
			fn to_term(&self, env:& Environment) -> Term {
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


// Tuple implementation

// macro_rules! tuple {
// 	() => ();
// 	( $($name:ident,)+) => (
// //		impl<$($name:Decodable),*> Decodable for ($($name,)*) {
// 		impl Decodable for ($($name,)*) {
// //	        #[allow(non_snake_case)]
// 			fn from_term(env:&Environment, term:Term) -> Result<Self, NifError> {
// 				unsafe {
// 					let mut arity:c_int = std::mem::uninitialized();
// 					let mut array:*const l1::ERL_NIF_TERM = std::mem::uninitialized();
// 					match enif_get_tuple(transmute(env), transmute(term), &arity, &array)
// 				}
// 				let ret = ($(try!(blah $name)))


// 	        fn decode<D: Decoder>(d: &mut D) -> Result<($($name,)*), D::Error> {
// 	            let len: uint = count_idents!($($name,)*);
// 	            d.read_tuple(len, |d| {
// 	                let mut i = 0;
// 	                let ret = ($(try!(d.read_tuple_arg({ i+=1; i-1 },
// 	                                                   |d| -> Result<$name,D::Error> {
// 	                    Decodable::decode(d)
// 	                })),)*);
// 	                return Ok(ret);
// 	            })
// 	        }
// 	    }
// 	    impl<$($name:Encodable),*> Encodable for ($($name,)*) {
// 	        #[allow(non_snake_case)]
// 	        fn encode<S: Encoder>(&self, s: &mut S) -> Result<(), S::Error> {
// 	            let ($(ref $name,)*) = *self;
// 	            let mut n = 0;
// 	            $(let $name = $name; n += 1;)*
// 	            s.emit_tuple(n, |s| {
// 	                let mut i = 0;
// 	                $(try!(s.emit_tuple_arg({ i+=1; i-1 }, |s| $name.encode(s)));)*
// 	                Ok(())
// 	            })
// 	        }
// 	    }
// 	    peel! { $($name,)* }
// 		)
// }

// let ret = ($(try!(d.read_tuple_arg({ i+=1; i-1 },
//                                                        |d| -> Result<$name,D::Error> {
//                         Decodable::decode(d)
//                     })),)*);
//                     return Ok(ret);

impl<T0: Decodable, T1: Decodable> Decodable for (T0,T1) {
	fn from_term(env:&Environment, term:Term) -> Result<Self, NifError> {
		unsafe {
			let mut arity:c_int = std::mem::uninitialized();
			let mut array:*const l1::ERL_NIF_TERM = std::mem::uninitialized();
			match l1::enif_get_tuple(transmute(env),
				 transmute(term), transmute(&arity), transmute(&array)) {
				0 => Err(NifError::Badarg),
				_ => {
						if arity != 2 {
							Err(NifError::Badarg)
						}
						else {
							Ok(( try!(Decodable::from_term(env, transmute(*(array.offset(0))))),
							     try!(Decodable::from_term(env, transmute(*(array.offset(1)))))
							  ))	
						}
					}
				}
			}
		}
	}


  // fn from_term(term: u32) -> Option<(T0,T1)> {
  //  let ts = [5,3];
  //  match TermTranscodeable::from_term(ts[0]) {
  //    Some(u0) =>
  //     match TermTranscodeable::from_term(ts[1]) {
  //     Some(u1) => Some((u0,u1)),
  //     _ => None,
  //    },
  //    _ => None,
  //  }
  // }
//   fn from_term(term: u32) -> Option<(T0,T1)> {
//      let ts = [5,3];
//      Some((
//       match TermTranscodeable::from_term(ts[0]) {
//         Some(u0) => u0,
//         _ => return None,
//       },
//       match TermTranscodeable::from_term(ts[1]) {
//         Some(u1) => u1,
//         _ => return None,
//       }
//       ))
//   }

//   fn to_term(&self) -> u32 {
//      6
//   }
// }




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
		extern "C" fn $wrapper(env: *mut ErlNifEnv,
		                          argc: c_int,
		                          args: *const ERL_NIF_TERM) -> ERL_NIF_TERM
		{
		    unsafe {
		         match $wrappee(&*env, std::slice::from_raw_parts(transmute(&args), argc as usize)) {
		            Ok(x) => transmute(x),
		            _ => enif_make_badarg(env),
		         }
		    }
		}
	)
}