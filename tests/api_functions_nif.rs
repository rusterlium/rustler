#[macro_use]
extern crate erlang_nif_sys;
use erlang_nif_sys::*;

extern "C" fn test_enif_make_int(env: *mut ErlNifEnv, _: c_int, _: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe { enif_make_int(env, -10) }
}

extern "C" fn test_enif_make_pid(env: *mut ErlNifEnv, _: c_int, _: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    let mut pid: ErlNifPid = unsafe { std::mem::uninitialized() };
    unsafe { enif_self(env, &mut pid) };
    enif_make_pid(env, &pid)
}

nif_init!(
    b"api_functions_nif\0",
    None, None, None, None,
    nif!(b"test_enif_make_int\0", 0, test_enif_make_int),
    nif!(b"test_enif_make_pid\0", 0, test_enif_make_pid)
);
