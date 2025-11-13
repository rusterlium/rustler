use crate::sys::{enif_compare_pids, enif_is_process_alive, enif_self};
use crate::wrapper::{pid, ErlNifPid};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::cmp::Ordering;
use std::mem::MaybeUninit;

#[derive(Copy, Clone)]
pub struct LocalPid {
    c: ErlNifPid,
}

impl LocalPid {
    #[inline]
    pub fn as_c_arg(&self) -> &ErlNifPid {
        &self.c
    }

    #[inline]
    pub fn from_c_arg(erl_nif_pid: ErlNifPid) -> Self {
        LocalPid { c: erl_nif_pid }
    }

    /// Check whether the given process is alive
    pub fn is_alive(self, env: Env) -> bool {
        env.is_process_alive(self)
    }
}

impl<'a> Decoder<'a> for LocalPid {
    #[inline]
    fn decode(term: Term<'a>) -> NifResult<LocalPid> {
        unsafe { pid::get_local_pid(term.get_env().as_c_arg(), term.as_c_arg()) }
            .map(|pid| LocalPid { c: pid })
            .ok_or(Error::BadArg)
    }
}

impl Encoder for LocalPid {
    #[inline]
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe { Term::new(env, pid::make_pid(env.as_c_arg(), self.c)) }
    }
}

impl PartialEq for LocalPid {
    fn eq(&self, other: &Self) -> bool {
        unsafe { enif_compare_pids(self.as_c_arg(), other.as_c_arg()) == 0 }
    }
}

impl Eq for LocalPid {}

impl PartialOrd for LocalPid {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LocalPid {
    fn cmp(&self, other: &Self) -> Ordering {
        let cmp = unsafe { enif_compare_pids(self.as_c_arg(), other.as_c_arg()) };
        cmp.cmp(&0)
    }
}

/// Caller information for async tasks with type-safe message sending.
///
/// Contains the calling process's PID and the task reference. When used as the first
/// parameter of a `#[rustler::task]`, it is automatically populated and provides
/// convenient methods for sending messages tagged with the task reference.
///
/// The generic type `T` is automatically inferred from the task's return type,
/// ensuring that intermediate messages sent via `send()` are the same type as
/// the final result.
///
/// # Example
///
/// ```ignore
/// #[rustler::task]
/// async fn with_progress(caller: Caller, work: Vec<i64>) -> Result<i64, String> {
///     for (i, item) in work.iter().enumerate() {
///         // Type-checked: must send Result<i64, String>
///         caller.send(Ok(i as i64));
///         process(item).await?;
///     }
///     Ok(work.len() as i64)
/// }
/// ```
#[cfg(feature = "tokio_rt")]
#[derive(Clone)]
pub struct Caller<T> {
    pid: LocalPid,
    task_ref: crate::ResourceArc<crate::TaskRef>,
    _phantom: std::marker::PhantomData<T>,
}

#[cfg(feature = "tokio_rt")]
impl<T: Encoder> Caller<T> {
    /// Create a new Caller.
    ///
    /// This is only used internally by the task macro.
    #[doc(hidden)]
    pub fn new(pid: LocalPid, task_ref: crate::ResourceArc<crate::TaskRef>) -> Self {
        Self {
            pid,
            task_ref,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Get the calling process's PID.
    pub fn pid(&self) -> &LocalPid {
        &self.pid
    }

    /// Get the task reference.
    pub fn task_ref(&self) -> &crate::ResourceArc<crate::TaskRef> {
        &self.task_ref
    }

    /// Send an intermediate message to the caller, automatically tagged with the task reference.
    ///
    /// The message will be sent as `{task_ref, message}`.
    ///
    /// The message type `T` must match the task's return type, ensuring type safety
    /// for all messages sent during task execution.
    ///
    /// # Example
    ///
    /// ```ignore
    /// #[rustler::task]
    /// async fn process(caller: Caller, count: i64) -> String {
    ///     for i in 0..count {
    ///         caller.send(format!("Progress: {}", i));  // ✅ Type-safe
    ///         // caller.send(i);  // ❌ Compile error: expected String, got i64
    ///     }
    ///     "Done".to_string()
    /// }
    /// ```
    pub fn send(&self, message: T) {
        let mut env = crate::OwnedEnv::new();
        let task_ref = self.task_ref.clone();
        let _ = env.send_and_clear(&self.pid, move |env| (task_ref, message).encode(env));
    }

    /// Send the final message and complete the task.
    ///
    /// This is used internally by the `#[rustler::task]` macro to send the
    /// task's return value. User code should just return the value normally.
    #[doc(hidden)]
    pub fn finish(self, message: T) {
        self.send(message);
    }

    /// Check whether the calling process is alive.
    pub fn is_alive(&self, env: Env) -> bool {
        self.pid.is_alive(env)
    }
}

impl Env<'_> {
    /// Return the calling process's pid.
    ///
    /// # Panics
    ///
    /// Panics if this environment is process-independent.  (The only way to get such an
    /// environment is to use `OwnedEnv`.  The `Env` that Rustler passes to NIFs when they're
    /// called is always associated with the calling Erlang process.)
    #[inline]
    pub fn pid(self) -> LocalPid {
        let mut pid = MaybeUninit::uninit();
        if unsafe { enif_self(self.as_c_arg(), pid.as_mut_ptr()) }.is_null() {
            panic!("environment is process-independent");
        }
        LocalPid {
            c: unsafe { pid.assume_init() },
        }
    }

    /// Checks whether the given process is alive
    pub fn is_process_alive(self, pid: LocalPid) -> bool {
        let res = unsafe { enif_is_process_alive(self.as_c_arg(), pid.as_c_arg()) };
        res != 0
    }
}
