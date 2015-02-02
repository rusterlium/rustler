
use raw;
use std;
use std::marker::ContravariantLifetime;
use std::mem::transmute;

pub use raw::ErlNifEnv as Environment;

#[derive(Copy)]
#[repr(C)]
pub struct Term<'a> {
	term: raw::ERL_NIF_TERM,
	marker: ContravariantLifetime<'a>,
}


// pub extern "C" fn wrapper(env: *mut ErlNifEnv,
//                           argc: c_int,
//                           args: *const ERL_NIF_TERM) -> ERL_NIF_TERM
// {
// 	let sargs = std::slice::from_raw_buf(args, argc);
// 	let result = wrapee(&*env, sargs);
// 	std::mem::transmute<ErlNifTerm, ERL_NIF_TERM> result
// }

pub fn make_int(env: & Environment, val: i32) -> Term {
	unsafe {
		transmute(raw::enif_make_int(transmute(env), val)	)
	}
}
