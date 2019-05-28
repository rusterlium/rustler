use crate::wrapper::ErlNifTaskFlags;
use crate::Env;

pub enum SchedulerFlags {
    Normal = ErlNifTaskFlags::ERL_NIF_NORMAL_JOB as isize,
    DirtyCpu = ErlNifTaskFlags::ERL_NIF_DIRTY_JOB_CPU_BOUND as isize,
    DirtyIo = ErlNifTaskFlags::ERL_NIF_DIRTY_JOB_IO_BOUND as isize,
}

pub fn consume_timeslice(env: Env, percent: i32) -> bool {
    let success = unsafe { erl_nif_sys::enif_consume_timeslice(env.as_c_arg(), percent) };
    success == 1
}
