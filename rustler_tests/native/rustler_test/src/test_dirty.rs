use rustler::{Atom, Encoder, Env, Yield};
use std::time::Duration;

mod atoms {
    rustler::atoms! { ok }
}

// TODO: Make these realistic

#[rustler::nif(schedule = "DirtyCpu")]
pub fn dirty_cpu() -> Atom {
    let duration = Duration::from_millis(100);
    std::thread::sleep(duration);

    atoms::ok()
}

#[rustler::nif(schedule = "DirtyIo")]
pub fn dirty_io() -> Atom {
    let duration = Duration::from_millis(100);
    std::thread::sleep(duration);

    atoms::ok()
}

#[rustler::nif]
fn yield_resume(input: i32) -> Atom {
    assert_eq!(input, 42);
    atoms::ok()
}

#[rustler::nif]
pub fn yields(env: Env) -> Yield {
    let term = 42.encode(env);
    Yield::to(yield_resume, vec![term])
}
