use rustler::{Atom, Env, Term};

use std::time;

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
    }
}

// TODO: Make these realistic

pub fn dirty_cpu<'a>(_env: Env<'a>, _: &[Term<'a>]) -> Atom {
    let duration = time::Duration::from_millis(100);
    ::std::thread::sleep(duration);

    atoms::ok()
}

pub fn dirty_io<'a>(_env: Env<'a>, _: &[Term<'a>]) -> Atom {
    let duration = time::Duration::from_millis(100);
    ::std::thread::sleep(duration);

    atoms::ok()
}
