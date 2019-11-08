use rustler::Atom;
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
