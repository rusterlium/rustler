use std::mem::MaybeUninit;

use crate::sys::{enif_get_local_pid, enif_make_pid, ErlNifEnv, ErlNifPid, ERL_NIF_TERM};

pub unsafe fn get_local_pid(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> Option<ErlNifPid> {
    let mut pid = MaybeUninit::uninit();
    if enif_get_local_pid(env, term, pid.as_mut_ptr()) == 0 {
        return None;
    }
    Some(pid.assume_init())
}

// pub unsafe fn is_process_alive(env: *mut ErlNifEnv, pid: &ErlNifPid) -> bool {
//     enif_is_process_alive(env, pid) != 0
// }

pub unsafe fn make_pid(env: *mut ErlNifEnv, pid: ErlNifPid) -> ERL_NIF_TERM {
    enif_make_pid(env, pid)
}
