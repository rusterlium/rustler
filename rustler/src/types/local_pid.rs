use crate::wrapper::{pid, ErlNifPid};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::cmp::Ordering;
use std::mem::MaybeUninit;

#[derive(Copy, Clone)]
pub struct LocalPid {
    c: ErlNifPid,
}

impl LocalPid {
    pub fn as_c_arg(&self) -> &ErlNifPid {
        &self.c
    }

    pub fn from_c_arg(erl_nif_pid: ErlNifPid) -> Self {
        LocalPid { c: erl_nif_pid }
    }

    /// Check whether the given process is alive
    pub fn is_alive(self, env: Env) -> bool {
        env.is_process_alive(self)
    }
}

impl<'a> Decoder<'a> for LocalPid {
    fn decode(term: Term<'a>) -> NifResult<LocalPid> {
        unsafe { pid::get_local_pid(term.get_env().as_c_arg(), term.as_c_arg()) }
            .map(|pid| LocalPid { c: pid })
            .ok_or(Error::BadArg)
    }
}

impl Encoder for LocalPid {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe { Term::new(env, pid::make_pid(env.as_c_arg(), self.c)) }
    }
}

impl PartialEq for LocalPid {
    fn eq(&self, other: &Self) -> bool {
        unsafe { rustler_sys::enif_compare_pids(self.as_c_arg(), other.as_c_arg()) == 0 }
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
        let cmp = unsafe { rustler_sys::enif_compare_pids(self.as_c_arg(), other.as_c_arg()) };
        cmp.cmp(&0)
    }
}

impl<'a> Env<'a> {
    /// Return the calling process's pid.
    ///
    /// # Panics
    ///
    /// Panics if this environment is process-independent.  (The only way to get such an
    /// environment is to use `OwnedEnv`.  The `Env` that Rustler passes to NIFs when they're
    /// called is always associated with the calling Erlang process.)
    pub fn pid(self) -> LocalPid {
        let mut pid = MaybeUninit::uninit();
        if unsafe { rustler_sys::enif_self(self.as_c_arg(), pid.as_mut_ptr()) }.is_null() {
            panic!("environment is process-independent");
        }
        LocalPid {
            c: unsafe { pid.assume_init() },
        }
    }

    /// Checks whether the given process is alive
    pub fn is_process_alive(self, pid: LocalPid) -> bool {
        let res = unsafe { rustler_sys::enif_is_process_alive(self.as_c_arg(), pid.as_c_arg()) };
        res != 0
    }
}
