//! A small NIF library whose tests are in Rust. `cargo test` runs them inside
//! a BEAM, through `tests/beam.rs`.

use std::sync::atomic::{AtomicI64, Ordering};
use std::thread;

use rustler::{Atom, Env, LocalPid, OwnedEnv, Resource, ResourceArc};

mod atoms {
    rustler::atoms! { ok, factorial, hello }
}

pub struct Counter(AtomicI64);

#[rustler::resource_impl]
impl Resource for Counter {}

impl Counter {
    fn add(&self, n: i64) -> i64 {
        self.0.fetch_add(n, Ordering::Relaxed) + n
    }
}

#[rustler::nif]
fn counter_new(start: i64) -> ResourceArc<Counter> {
    ResourceArc::new(Counter(AtomicI64::new(start)))
}

#[rustler::nif]
fn counter_add(counter: ResourceArc<Counter>, n: i64) -> i64 {
    counter.add(n)
}

fn factorial(n: u64) -> u64 {
    (1..=n).product()
}

/// Computes `n!` on a thread of its own and sends `{factorial, Result}` to
/// `pid`.
fn send_factorial(pid: LocalPid, n: u64) {
    thread::spawn(move || {
        let _ = OwnedEnv::new().send_and_clear(&pid, |_| (atoms::factorial(), factorial(n)));
    });
}

#[rustler::nif]
fn factorial_async(env: Env, n: u64) -> Atom {
    send_factorial(env.pid(), n);
    atoms::ok()
}

rustler::init!("Elixir.BeamTestDemo");

#[cfg(feature = "beam_test")]
mod ported;
#[cfg(feature = "beam_test")]
mod tests;
