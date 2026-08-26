/// True cooperative yielding NIFs using enif_schedule_nif
///
/// This approach makes NIF calls appear synchronous to Elixir while yielding internally.
use crate::codegen_runtime::NifReturned;
use crate::schedule::SchedulerFlags;
use crate::wrapper::NIF_TERM;
use crate::{Encoder, Env, ResourceArc};
use std::ffi::CString;
use std::future::Future;
use std::pin::Pin;
use std::sync::Mutex;
use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

// Type-erased poll function that takes Env and returns encoded result
type PollFn = dyn FnMut(&mut Context<'_>, Env) -> Poll<NIF_TERM> + Send;

/// Saved state for a yielding computation
pub struct YieldingTaskState {
    /// Type-erased future polling function
    poll_fn: Mutex<Pin<Box<PollFn>>>,
}

impl crate::Resource for YieldingTaskState {}

// Auto-register the resource
crate::codegen_runtime::inventory::submit! {
    crate::resource::Registration::new::<YieldingTaskState>()
}

/// Run a future cooperatively, yielding to the BEAM scheduler as needed.
///
/// This is the main entry point for yielding NIFs. Call this with your async code
/// and it will handle yielding automatically.
///
/// # Example
///
/// ```ignore
/// use rustler::codegen_runtime::NifReturned;
///
/// #[rustler::nif]
/// fn my_yielding_nif(env: Env) -> NifReturned {
///     yielding_nif_run(env, async {
///         // Your async code here - yields automatically
///         let mut sum = 0;
///         for i in 0..1000 {
///             sum += i;
///             // Yield periodically to avoid blocking
///             yield_now().await;
///         }
///         sum
///     })
/// }
/// ```
///
/// From Elixir, this appears as a normal blocking call:
/// ```elixir
/// result = MyNif.my_yielding_nif()  # Blocks cooperatively until done
/// ```
pub fn yielding_nif_run<F, T>(env: Env, future: F) -> NifReturned
where
    F: Future<Output = T> + Send + 'static,
    T: Encoder + Send + 'static,
{
    start_yielding(env, future)
}

/// Internal function for managing continuation state.
///
/// This should not be called directly by users.
pub fn yielding_nif<F, T>(
    env: Env,
    state: Option<ResourceArc<YieldingTaskState>>,
    future: F,
) -> NifReturned
where
    F: Future<Output = T> + Send + 'static,
    T: Encoder + Send + 'static,
{
    match state {
        None => {
            // Initial call - create state and start
            start_yielding(env, future)
        }
        Some(state_resource) => {
            // Continuation - resume from state
            resume_yielding(env, state_resource)
        }
    }
}

/// Start a new yielding computation
fn start_yielding<F, T>(env: Env, future: F) -> NifReturned
where
    F: Future<Output = T> + Send + 'static,
    T: Encoder + Send + 'static,
{
    // Box and pin the future
    let mut future = Box::pin(future);

    // Create type-erased poll function
    let poll_fn: Pin<Box<PollFn>> =
        Box::pin(
            move |ctx: &mut Context<'_>, env: Env| match future.as_mut().poll(ctx) {
                Poll::Ready(result) => Poll::Ready(result.encode(env).as_c_arg()),
                Poll::Pending => Poll::Pending,
            },
        );

    // Create task state resource
    let task_state = YieldingTaskState {
        poll_fn: Mutex::new(poll_fn),
    };
    let resource = ResourceArc::new(task_state);

    // Poll immediately
    poll_and_return(env, resource)
}

/// Resume a yielding computation from saved state
fn resume_yielding(env: Env, state: ResourceArc<YieldingTaskState>) -> NifReturned {
    poll_and_return(env, state)
}

/// Poll the future and return appropriate NifReturned
fn poll_and_return(env: Env, state: ResourceArc<YieldingTaskState>) -> NifReturned {
    // Create a simple waker that does nothing (we'll poll again on reschedule)
    let waker = noop_waker();
    let mut context = Context::from_waker(&waker);

    // Poll the future first - don't check timeslice before giving it a chance to complete
    let result = {
        let mut poll_fn = state
            .poll_fn
            .lock()
            .expect("YieldingTaskState mutex poisoned");

        // SAFETY: We're not moving the function, just calling it
        let f = unsafe { poll_fn.as_mut().get_unchecked_mut() };
        f(&mut context, env)
    };

    match result {
        Poll::Ready(term) => {
            // Future completed - return result
            NifReturned::Term(term)
        }
        Poll::Pending => {
            // Future still running - check if we should yield
            // Consume a small amount of timeslice (10%) and check if we should continue
            if crate::schedule::consume_timeslice(env, 10) {
                // Still have timeslice - could poll again immediately
                // But for now, let's reschedule to give other work a chance
                reschedule_continuation(env, state)
            } else {
                // Timeslice exhausted - definitely reschedule
                reschedule_continuation(env, state)
            }
        }
    }
}

/// Reschedule the continuation to run again
fn reschedule_continuation(env: Env, state: ResourceArc<YieldingTaskState>) -> NifReturned {
    // Encode the state resource as an argument for the continuation
    let state_term = state.encode(env).as_c_arg();

    NifReturned::Reschedule {
        fun_name: CString::new("__yielding_continuation").unwrap(),
        flags: SchedulerFlags::Normal,
        fun: yielding_continuation_raw,
        args: vec![state_term],
    }
}

/// Raw C-ABI continuation function called by enif_schedule_nif
unsafe extern "C" fn yielding_continuation_raw(
    env_ptr: *mut crate::sys::ErlNifEnv,
    argc: i32,
    argv: *const NIF_TERM,
) -> NIF_TERM {
    // Create Env from the pointer
    let env = Env::new_internal(&env_ptr, env_ptr, crate::env::EnvKind::Callback);

    // Decode the state resource from argv[0]
    if argc != 1 {
        return env.error_tuple("Expected 1 argument").as_c_arg();
    }

    let state_term = crate::Term::new(env, *argv);

    match state_term.decode::<ResourceArc<YieldingTaskState>>() {
        Ok(state) => {
            // Resume the computation
            match resume_yielding(env, state) {
                NifReturned::Term(term) => term,
                NifReturned::Reschedule {
                    fun_name,
                    flags,
                    fun,
                    args,
                } => {
                    // Call enif_schedule_nif to reschedule again
                    unsafe {
                        crate::sys::enif_schedule_nif(
                            env_ptr,
                            fun_name.as_ptr(),
                            flags as i32,
                            fun,
                            args.len() as i32,
                            args.as_ptr(),
                        )
                    }
                }
                NifReturned::BadArg => crate::types::atom::error().encode(env).as_c_arg(),
                NifReturned::Raise(term) => term,
            }
        }
        Err(_) => {
            // Failed to decode state - return error
            env.error_tuple("Invalid task state").as_c_arg()
        }
    }
}

/// Create a no-op waker
///
/// Since we're using cooperative yielding with enif_schedule_nif, we don't need
/// the waker to do anything. We'll poll again when we're rescheduled.
fn noop_waker() -> Waker {
    fn noop_clone(_: *const ()) -> RawWaker {
        noop_raw_waker()
    }
    fn noop(_: *const ()) {}

    fn noop_raw_waker() -> RawWaker {
        RawWaker::new(
            std::ptr::null(),
            &RawWakerVTable::new(noop_clone, noop, noop, noop),
        )
    }

    unsafe { Waker::from_raw(noop_raw_waker()) }
}

/// A simple future that yields once before completing.
///
/// This is useful for inserting yield points in your async code to check
/// the timeslice and give the scheduler a chance to run other work.
///
/// # Example
///
/// ```ignore
/// async fn process_large_file(path: String) -> Result<Vec<u8>> {
///     let mut buffer = Vec::new();
///     let mut file = std::fs::File::open(path)?;
///
///     loop {
///         let mut chunk = vec![0u8; 4096];
///         match file.read(&mut chunk)? {
///             0 => break,
///             n => {
///                 buffer.extend_from_slice(&chunk[..n]);
///                 // Yield to scheduler periodically
///                 yield_now().await;
///             }
///         }
///     }
///
///     Ok(buffer)
/// }
/// ```
pub struct YieldNow {
    yielded: bool,
}

impl Future for YieldNow {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.yielded {
            Poll::Ready(())
        } else {
            self.yielded = true;
            Poll::Pending
        }
    }
}

/// Yield control back to the BEAM scheduler.
///
/// This returns a future that yields once before completing, allowing
/// the scheduler to run other work if needed.
pub fn yield_now() -> YieldNow {
    YieldNow { yielded: false }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yield_now_completes() {
        // YieldNow should return Pending once, then Ready
        let mut future = Box::pin(yield_now());
        let waker = noop_waker();
        let mut ctx = Context::from_waker(&waker);

        assert!(matches!(future.as_mut().poll(&mut ctx), Poll::Pending));
        assert!(matches!(future.as_mut().poll(&mut ctx), Poll::Ready(())));
    }
}
