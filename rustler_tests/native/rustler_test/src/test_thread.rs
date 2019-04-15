use rustler::thread;
use rustler::types::atom;
use rustler::{Encoder, Env, NifResult, Term};
use std;

pub fn threaded_fac<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    // Multiply two numbers; panic on overflow. In Rust, the `*` operator wraps (rather than
    // panicking) in release builds. A test depends on this panicking, so we make sure it panics in
    // all builds. The test also checks the panic message.
    fn mul(a: u64, b: u64) -> u64 {
        a.checked_mul(b).expect("threaded_fac: integer overflow")
    }

    let n: u64 = args[0].decode()?;
    thread::spawn::<thread::ThreadSpawner, _>(env, move |thread_env| {
        let result = (1..n + 1).fold(1, mul);
        result.encode(thread_env)
    });

    Ok(atom::ok().to_term(env))
}

pub fn threaded_sleep<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let msec: u64 = args[0].decode()?;

    let q = msec / 1000;
    let r = (msec % 1000) as u32;
    thread::spawn::<thread::ThreadSpawner, _>(env, move |thread_env| {
        std::thread::sleep(std::time::Duration::new(q as u64, r * 1_000_000));
        msec.encode(thread_env)
    });

    Ok(atom::ok().to_term(env))
}
