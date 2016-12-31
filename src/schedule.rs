use super::NifEnv;
use super::wrapper::nif_interface::enif_consume_timeslice;

pub fn consume_timeslice<'a>(env: &'a NifEnv, percent: i32) -> bool {
    let success = unsafe { enif_consume_timeslice(env.as_c_arg(), percent) };
    success == 1
}
