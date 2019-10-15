use crate::env::OwnedEnv;
use crate::types::atom::Atom;
use crate::{Encoder, Env, Term};
use std::panic;
use std::thread;

/// A `JobSpawner` is a value that can run Rust code on non-Erlang system threads.
/// Abstracts away details of thread management for `spawn()`.
///
/// Note: Implementations of `spawn()` must call the closure on a thread that is **not** managed by
/// the Erlang VM's scheduler. Otherwise, `rustler::thread::spawn()` would try to send a message
/// from an `OwnedEnv` on an Erlang thread, which would panic.
pub trait JobSpawner {
    /// Run the given closure on another thread.
    fn spawn<F: FnOnce() + Send + panic::UnwindSafe + 'static>(job: F);
}

/// A `JobSpawner` that uses a separate system thread for each job.
pub struct ThreadSpawner;

impl JobSpawner for ThreadSpawner {
    /// This delegates to `std::thread::spawn()`.
    fn spawn<F: FnOnce() + Send + panic::UnwindSafe + 'static>(job: F) {
        thread::spawn(job);
    }
}

/// Implements threaded NIFs.
///
/// This spawns a thread that calls the given closure `thread_fn`. When the closure returns, the
/// thread sends its return value back to the calling process.  If the closure panics, an `{error,
/// Reason}` tuple is sent instead.
///
/// Note that the thread creates a new `Env` and passes it to the closure, so the closure
/// runs under a separate environment, not under `env`.
///
pub fn spawn<'a, S, F>(env: Env<'a>, thread_fn: F)
where
    F: for<'b> FnOnce(Env<'b>) -> Term<'b> + Send + panic::UnwindSafe + 'static,
    S: JobSpawner,
{
    let pid = env.pid();
    S::spawn(move || {
        OwnedEnv::new().send_and_clear(&pid, |env| {
            match panic::catch_unwind(|| thread_fn(env)) {
                Ok(term) => term,
                Err(err) => {
                    // Try to get an error message from Rust.
                    let reason = if let Some(string) = err.downcast_ref::<String>() {
                        string.encode(env)
                    } else if let Some(&s) = err.downcast_ref::<&'static str>() {
                        s.encode(env)
                    } else {
                        Atom::from_bytes(env, b"nif_panic")
                            .ok()
                            .unwrap()
                            .to_term(env)
                    };
                    env.error_tuple(reason)
                }
            }
        });
    });
}
