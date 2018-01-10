use rustler::Encoder;
use rustler::{NifEnv, NifTerm, NifResult};

use std::time;

mod atoms {
    rustler_atoms! {
        atom ok;
    }
}

// TODO: Make these realistic

pub fn dirty_cpu<'a>(env: NifEnv<'a>, _: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let duration = time::Duration::from_millis(100);
    ::std::thread::sleep(duration);

    Ok(atoms::ok().encode(env))
}

pub fn dirty_io<'a>(env: NifEnv<'a>, _: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let duration = time::Duration::from_millis(100);
    ::std::thread::sleep(duration);

    Ok(atoms::ok().encode(env))
}
