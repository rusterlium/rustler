use crate::sys::{enif_compare_pids, enif_is_process_alive, enif_self};
use crate::wrapper::{pid, ErlNifPid};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::cmp::Ordering;
use std::mem::MaybeUninit;

#[derive(Copy, Clone)]
pub struct LocalPid {
    c: ErlNifPid,
}

// Safe: LocalPid is just a process identifier that can be safely sent across threads.
// PIDs are used for message passing in BEAM, which is inherently thread-safe.
unsafe impl Send for LocalPid {}
unsafe impl Sync for LocalPid {}

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
