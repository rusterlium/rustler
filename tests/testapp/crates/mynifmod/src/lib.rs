
#[macro_use]
extern crate erlang_nif_sys;
use erlang_nif_sys::*;

use std::{mem, ptr};

static mut RUSTMAP_TYPE: *const ErlNifResourceType = 0 as *const ErlNifResourceType;

nif_init!("mynifmod", [
	("times2", 1, slice_args!(times2)),
	("test_enif_make_pid", 0, test_enif_make_pid),
	("rustmap", 0, rustmap),
	],
	{load: mynifmod_load});

unsafe fn mynifmod_load(env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
	let mut tried: ErlNifResourceFlags = mem::uninitialized();
	RUSTMAP_TYPE = enif_open_resource_type(
		env,
		ptr::null(),
		b"rustmap\0".as_ptr(),
		Some(rustmap_destructor),
		ErlNifResourceFlags::ERL_NIF_RT_CREATE,
		&mut tried);
	RUSTMAP_TYPE.is_null() as c_int
}

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

use std::collections::HashMap;
type RustMap = HashMap<String, String>;

unsafe extern "C" fn rustmap_destructor(_env: *mut ErlNifEnv, handle: *mut c_void) {
    ptr::read(handle as *mut RustMap);
}

unsafe fn rustmap(env: *mut ErlNifEnv, _: c_int, _: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    // Create a value with nontrivial destructor cleanup.
    let mut map = RustMap::new();
    map.insert("Rust".to_string(), "Erlang".to_string());
    map.insert("Erlang".to_string(), "Rust".to_string());

    let mem = enif_alloc_resource(RUSTMAP_TYPE, mem::size_of::<RustMap>());
    assert_eq!(mem as usize % mem::align_of::<RustMap>(), 0);
    ptr::write(mem as *mut RustMap, map);
    let term = enif_make_resource(env, mem);
    enif_release_resource(mem);
    term
}
