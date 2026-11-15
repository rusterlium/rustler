#![doc = include_str!("../README.md")]

use std::backtrace::{Backtrace, BacktraceStatus};
use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::fmt::Debug;
use std::panic::{self, AssertUnwindSafe};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, Once};

use rustler::codegen_runtime::{ErlNifEnv, ErlNifTerm};
use rustler::{Encoder, Env, Term};

pub use rustler_beam_test_macros::test;

mod atoms {
    rustler::atoms! { ok, failed, next_message, describe }
}

/// What a test function returns: `()`, a [`Receive`], or a `Result` of one of
/// them, whose `Err` fails the test.
pub trait Outcome {
    #[doc(hidden)]
    fn into_step(self) -> Step;
}

#[doc(hidden)]
pub enum Step {
    Passed,
    Failed(String),
    Receive(Receive),
}

/// The rest of a test, which runs with the next message that the process of
/// the test receives. See [`receive`].
pub struct Receive(Box<dyn for<'a> FnOnce(Env<'a>, Term<'a>) -> Step + Send>);

/// Continues the test with the next message that its process receives.
///
/// A NIF cannot read the mailbox of its process, so the test returns, the
/// process receives one message, and `then` runs in a second NIF call of the
/// same process. No term of the first call is valid in the second one.
/// The test fails when no message arrives within 5 seconds.
pub fn receive<F, O>(then: F) -> Receive
where
    F: for<'a> FnOnce(Env<'a>, Term<'a>) -> O + Send + 'static,
    O: Outcome,
{
    Receive(Box::new(move |env, message| then(env, message).into_step()))
}

impl Outcome for () {
    fn into_step(self) -> Step {
        Step::Passed
    }
}

impl Outcome for Receive {
    fn into_step(self) -> Step {
        Step::Receive(self)
    }
}

impl<O: Outcome, E: Debug> Outcome for Result<O, E> {
    fn into_step(self) -> Step {
        match self {
            Ok(outcome) => outcome.into_step(),
            Err(error) => Step::Failed(format!("returned an error: {error:?}")),
        }
    }
}

/// What `#[ignore]` and `#[should_panic]` say about a test.
#[doc(hidden)]
pub struct Test {
    pub ignore: bool,
    /// The text that the message of the panic must contain.
    pub should_panic: Option<&'static str>,
}

/// The tests that wait for a message, by the number that the driver passes
/// back with it.
static WAITING: Mutex<BTreeMap<u64, Receive>> = Mutex::new(BTreeMap::new());
static NEXT: AtomicU64 = AtomicU64::new(0);

/// One call of a test NIF. Its argument is `describe`, `start`, or
/// `{Continuation, Message}` after it returned `{next_message, Continuation}`.
/// For `describe` it returns whether the test is ignored. Else it returns
/// `ok`, `{failed, Report}`, or `{next_message, Continuation}`.
///
/// # Safety
///
/// `env` and `argv` are the arguments of a NIF call with one argument.
#[doc(hidden)]
pub unsafe fn step<O: Outcome>(
    env: *mut ErlNifEnv,
    argv: *const ErlNifTerm,
    test: &Test,
    run: impl for<'a> FnOnce(Env<'a>) -> O,
) -> ErlNifTerm {
    let lifetime = ();
    let env = Env::new(&lifetime, env);
    let argument = Term::new(env, *argv);
    if argument == atoms::describe().to_term(env) {
        return test.ignore.encode(env).as_c_arg();
    }

    let caught = catch_panic(|| match argument.decode::<(u64, Term)>() {
        Ok((continuation, message)) => {
            let Receive(then) = WAITING
                .lock()
                .unwrap()
                .remove(&continuation)
                .expect("the driver passes each continuation back once");
            then(env, message)
        }
        Err(_) => run(env).into_step(),
    });

    let step = match (caught, test.should_panic) {
        (Err(panic), None) => Step::Failed(panic.report),
        (Err(panic), Some(expected)) if panic.message.contains(expected) => Step::Passed,
        (Err(panic), Some(expected)) => Step::Failed(format!(
            "{}\nnote: panic did not contain expected string\n expected substring: {expected:?}",
            panic.report
        )),
        (Ok(Step::Passed), Some(_)) => Step::Failed("note: test did not panic as expected".into()),
        (Ok(step), _) => step,
    };
    match step {
        Step::Passed => atoms::ok().encode(env),
        Step::Failed(report) => (atoms::failed(), report).encode(env),
        Step::Receive(receive) => {
            let continuation = NEXT.fetch_add(1, Ordering::Relaxed);
            WAITING.lock().unwrap().insert(continuation, receive);
            (atoms::next_message(), continuation).encode(env)
        }
    }
    .as_c_arg()
}

thread_local! {
    /// Whether this thread runs a test, so that the panic hook keeps the panic.
    static IN_TEST: Cell<bool> = const { Cell::new(false) };
    /// The message, the location, and the backtrace of the last panic of a
    /// test on this thread.
    static SEEN: RefCell<Option<(String, String, String)>> = const { RefCell::new(None) };
}

struct Panic {
    message: String,
    /// The message with its location, as `cargo test` shows a panic.
    report: String,
}

/// Runs a part of a test and catches its panic.
fn catch_panic(part: impl FnOnce() -> Step) -> Result<Step, Panic> {
    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        let default = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if !IN_TEST.get() {
                return default(info);
            }
            let message = info.payload_as_str().unwrap_or("Box<dyn Any>").to_string();
            let location = info.location().map(|at| format!(" at {at}"));
            let mut backtrace = String::new();
            let captured = Backtrace::capture();
            if captured.status() == BacktraceStatus::Captured {
                backtrace = format!("\nstack backtrace:\n{captured}");
            }
            SEEN.set(Some((message, location.unwrap_or_default(), backtrace)));
        }));
    });

    IN_TEST.set(true);
    SEEN.take();
    let caught = panic::catch_unwind(AssertUnwindSafe(part));
    IN_TEST.set(false);
    caught.map_err(|payload| {
        let message = payload
            .downcast_ref::<String>()
            .map(String::as_str)
            .or_else(|| payload.downcast_ref::<&str>().copied())
            .unwrap_or("Box<dyn Any>");
        // The hook saw another panic, or none, when `resume_unwind` passes
        // one on, for example from a thread that the test joined.
        let (_, location, backtrace) = SEEN
            .take()
            .filter(|(seen, ..)| seen == message)
            .unwrap_or_default();
        Panic {
            message: message.to_string(),
            report: format!("panicked in the BEAM{location}:\n{message}{backtrace}"),
        }
    })
}
