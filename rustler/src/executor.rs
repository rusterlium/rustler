//! Explicit handoff of owned work to an external async executor.

use std::fmt;
use std::future::Future;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Waker};

/// A task submitted to an external executor.
pub type ExecutorTask = Pin<Box<dyn Future<Output = ()> + Send + 'static>>;

/// Adapter for an executor with its own I/O and timer drivers.
///
/// Accepted tasks must be polled under that executor's runtime context, not on
/// the BEAM scheduler. Dropping a task reports executor shutdown to its caller.
pub trait Executor: Send + Sync {
    fn spawn(&self, task: ExecutorTask) -> Result<(), ExecutorError>;
}

/// An external task could not complete.
#[derive(Debug)]
pub enum ExecutorError {
    /// The adapter rejected the task.
    Spawn(String),
    /// The task or its destructor panicked.
    Panicked,
    /// The executor dropped the task without completing it.
    Stopped,
    /// Handoff was attempted outside an executor-enabled async NIF.
    NotEnabled,
}

impl fmt::Display for ExecutorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Spawn(reason) => write!(f, "executor rejected task: {reason}"),
            Self::Panicked => f.write_str("executor task panicked"),
            Self::Stopped => f.write_str("executor stopped before task completed"),
            Self::NotEnabled => f.write_str("on_executor requires #[rustler::nif(executor)]"),
        }
    }
}

impl std::error::Error for ExecutorError {}

impl From<ExecutorError> for crate::Error {
    fn from(error: ExecutorError) -> Self {
        Self::RaiseTerm(Box::new(error.to_string()))
    }
}

struct Completion<T> {
    result: Option<Result<T, ExecutorError>>,
    cancelled: bool,
    caller: Option<Waker>,
    task: Option<Waker>,
}

struct Work<F: Future> {
    future: Option<Pin<Box<F>>>,
    completion: Arc<Mutex<Completion<F::Output>>>,
    // Retain the continuation resource while external work is executing or
    // awaiting cancellation, even after the calling process has terminated.
    _keep_alive: crate::async_context::KeepAlive,
}

impl<F: Future> Work<F> {
    fn finish(&mut self, result: Result<F::Output, ExecutorError>) {
        let future = self.future.take();
        let dropped = catch_unwind(AssertUnwindSafe(|| drop(future)));
        let result = if dropped.is_err() {
            Err(ExecutorError::Panicked)
        } else {
            result
        };
        let caller = {
            let mut completion = self.completion.lock().unwrap();
            completion.task = None;
            if completion.cancelled {
                return;
            }
            completion.result = Some(result);
            completion.caller.take()
        };
        if let Some(caller) = caller {
            caller.wake();
        }
    }
}

impl<F: Future> Future for Work<F> {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<()> {
        let cancelled = {
            let mut completion = self.completion.lock().unwrap();
            completion.task = Some(context.waker().clone());
            completion.cancelled
        };
        if cancelled {
            self.finish(Err(ExecutorError::Stopped));
            return Poll::Ready(());
        }
        let result = catch_unwind(AssertUnwindSafe(|| {
            self.future
                .as_mut()
                .expect("executor task already completed")
                .as_mut()
                .poll(context)
        }));
        match result {
            Ok(Poll::Pending) => Poll::Pending,
            Ok(Poll::Ready(value)) => {
                self.finish(Ok(value));
                Poll::Ready(())
            }
            Err(_) => {
                self.finish(Err(ExecutorError::Panicked));
                Poll::Ready(())
            }
        }
    }
}

impl<F: Future> Drop for Work<F> {
    fn drop(&mut self) {
        if self.future.is_some() {
            self.finish(Err(ExecutorError::Stopped));
        }
    }
}

struct Wait<T>(Arc<Mutex<Completion<T>>>);

impl<T> Future for Wait<T> {
    type Output = Result<T, ExecutorError>;

    fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        let mut completion = self.0.lock().unwrap();
        match completion.result.take() {
            Some(result) => Poll::Ready(result),
            None => {
                completion.caller = Some(context.waker().clone());
                crate::async_context::request_executor_wait();
                Poll::Pending
            }
        }
    }
}

impl<T> Drop for Wait<T> {
    fn drop(&mut self) {
        let task = {
            let mut completion = self.0.lock().unwrap();
            completion.cancelled = true;
            completion.caller = None;
            completion.task.take()
        };
        if let Some(task) = task {
            task.wake();
        }
    }
}

/// Run owned work on an external executor, then resume the NIF on the BEAM.
///
/// Requires `#[rustler::nif(executor)]` and an Erlang-side wait wrapper.
/// The submitted future and its output must be `Send + 'static`: they cannot
/// borrow the NIF's environment, terms or binary slices.
///
/// Cancellation wakes the external task so the executor can drop its future.
/// It cannot forcibly interrupt blocking code or an executor that stops polling.
pub async fn on_executor<E, F>(executor: &E, future: F) -> Result<F::Output, ExecutorError>
where
    E: Executor + ?Sized,
    F: Future + Send + 'static,
    F::Output: Send + 'static,
{
    if !crate::async_context::executor_enabled() {
        return Err(ExecutorError::NotEnabled);
    }
    let completion = Arc::new(Mutex::new(Completion {
        result: None,
        cancelled: false,
        caller: None,
        task: None,
    }));
    let wait = Wait(completion.clone());
    executor.spawn(Box::pin(Work {
        future: Some(Box::pin(future)),
        completion,
        _keep_alive: crate::async_context::executor_keep_alive()
            .expect("missing async NIF continuation"),
    }))?;
    wait.await
}
