use rustler::thread;
use rustler::types::atom;
use rustler::{Atom, Encoder, Env};

#[rustler::nif]
pub fn threaded_fac(env: Env, n: u64) -> Atom {
    // Multiply two numbers; panic on overflow. In Rust, the `*` operator wraps (rather than
    // panicking) in release builds. A test depends on this panicking, so we make sure it panics in
    // all builds. The test also checks the panic message.
    fn mul(a: u64, b: u64) -> u64 {
        a.checked_mul(b).expect("threaded_fac: integer overflow")
    }

    thread::spawn::<thread::ThreadSpawner, _>(env, move |thread_env| {
        let result = (1..=n).fold(1, mul);
        result.encode(thread_env)
    });

    atom::ok()
}

#[rustler::nif]
pub fn threaded_sleep(env: Env, msec: u64) -> Atom {
    let q = msec / 1000;
    let r = (msec % 1000) as u32;
    thread::spawn::<thread::ThreadSpawner, _>(env, move |thread_env| {
        std::thread::sleep(std::time::Duration::new(q as u64, r * 1_000_000));
        msec.encode(thread_env)
    });

    atom::ok()
}
