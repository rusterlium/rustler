use crate::sys::enif_consume_timeslice;
use crate::Env;

#[derive(Clone, Copy, Debug)]
pub enum SchedulerFlags {
    Normal = crate::sys::ERL_NIF_NORMAL_JOB as isize,
    DirtyCpu = crate::sys::ERL_NIF_DIRTY_JOB_CPU_BOUND as isize,
    DirtyIo = crate::sys::ERL_NIF_DIRTY_JOB_IO_BOUND as isize,
}

pub fn consume_timeslice(env: Env, percent: i32) -> bool {
    let success = unsafe { enif_consume_timeslice(env.as_c_arg(), percent) };
    success == 1
}
