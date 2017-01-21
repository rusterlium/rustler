
#[macro_use]
extern crate erlang_nif_sys;
use erlang_nif_sys::*;

use std::mem;

nif_init!("mynifmod", [
	("times2", 1, slice_args!(times2)),
	("test_enif_make_pid", 0, test_enif_make_pid),
	]);


fn times2(env: *mut ErlNifEnv, args: &[ERL_NIF_TERM]) -> ERL_NIF_TERM {
	unsafe {
		let mut result: i32 = mem::uninitialized();
		if 1==args.len() && 0!=enif_get_int(env, args[0], &mut result) {
			enif_make_int(env, 2*result)
		}
		else {
			enif_make_badarg(env)
		}
	}
}

fn test_enif_make_pid(env: *mut ErlNifEnv, _: c_int, _: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut pid: ErlNifPid = unsafe { mem::uninitialized() };
    unsafe { enif_self(env, &mut pid) };
    unsafe { enif_make_pid(env, &pid) }
}
