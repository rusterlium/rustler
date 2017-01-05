use super::NifEnv;
use super::wrapper::nif_interface::enif_consume_timeslice;

pub enum NifScheduleFlags {
    NORMAL = 0,
    DIRTY_CPU = 1,
    DIRTY_IO = 2,
}

pub fn consume_timeslice<'a>(env: &'a NifEnv, percent: i32) -> bool {
    let success = unsafe { enif_consume_timeslice(env.as_c_arg(), percent) };
    success == 1
}
