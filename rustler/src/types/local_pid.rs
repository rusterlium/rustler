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

/// A wrapper for `LocalPid` that represents the calling process in async NIFs.
///
/// When used as the first parameter of an async NIF, `CallerPid` is automatically
/// populated with the calling process's PID, and is not decoded from the arguments.
/// This allows async NIFs to send intermediate messages back to the caller.
///
/// # Example
///
/// ```ignore
/// #[rustler::nif]
/// async fn with_progress(caller: CallerPid, work: Vec<i64>) -> i64 {
///     // Send progress updates
///     let mut env = OwnedEnv::new();
///     env.send(caller.as_pid(), |e| "started".encode(e));
///
///     let result = do_work(work).await;
///
///     // Final result sent automatically
///     result
/// }
/// ```
#[derive(Copy, Clone)]
pub struct CallerPid(LocalPid);

impl CallerPid {
    /// Create a new CallerPid from a LocalPid.
    ///
    /// This is only used internally by the NIF macro.
    #[doc(hidden)]
    pub fn new(pid: LocalPid) -> Self {
        CallerPid(pid)
    }

    /// Get the underlying LocalPid.
    pub fn as_pid(&self) -> &LocalPid {
        &self.0
    }

    /// Check whether the calling process is alive.
    pub fn is_alive(self, env: Env) -> bool {
        self.0.is_alive(env)
    }
}

impl std::ops::Deref for CallerPid {
    type Target = LocalPid;

    fn deref(&self) -> &Self::Target {
        &self.0
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
