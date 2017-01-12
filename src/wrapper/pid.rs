use super::nif_interface::{ self, NIF_ENV, NIF_TERM, ErlNifPid };
use std::mem;

pub unsafe fn get_local_pid(env: NIF_ENV, term: NIF_TERM) -> Option<ErlNifPid> {
    let mut pid: ErlNifPid = mem::uninitialized();
    if nif_interface::enif_get_local_pid(env, term, &mut pid) == 0 {
        return None;
    }
    Some(pid)
}

// pub unsafe fn is_process_alive(env: NIF_ENV, pid: &ErlNifPid) -> bool {
//     nif_interface::enif_is_process_alive(env, pid) != 0
// }

pub unsafe fn make_pid(env: NIF_ENV, pid: &ErlNifPid) -> NIF_TERM {
    nif_interface::enif_make_pid(env, pid)
}
