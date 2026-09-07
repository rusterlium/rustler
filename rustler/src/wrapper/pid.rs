use std::mem::MaybeUninit;

use crate::sys::{enif_get_local_pid, enif_make_pid, ErlNifEnv, ErlNifPid, ErlNifTerm};

pub unsafe fn get_local_pid(env: *mut ErlNifEnv, term: ErlNifTerm) -> Option<ErlNifPid> {
    let mut pid = MaybeUninit::uninit();
    if enif_get_local_pid(env, term, pid.as_mut_ptr()) == 0 {
        return None;
    }
    Some(pid.assume_init())
}

pub unsafe fn make_pid(env: *mut ErlNifEnv, pid: ErlNifPid) -> ErlNifTerm {
    enif_make_pid(env, pid)
}
