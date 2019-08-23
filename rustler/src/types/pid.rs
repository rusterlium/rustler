use crate::wrapper::{pid, ErlNifPid};
use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::mem;

#[derive(Clone)]
pub struct Pid {
    c: ErlNifPid,
}

impl Pid {
    pub fn as_c_arg(&self) -> &ErlNifPid {
        &self.c
    }
}

impl<'a> Decoder<'a> for Pid {
    fn decode(term: Term<'a>) -> NifResult<Pid> {
        unsafe { pid::get_local_pid(term.get_env().as_c_arg(), term.as_c_arg()) }
            .map(|pid| Pid { c: pid })
            .ok_or(Error::BadArg)
    }
}

impl Encoder for Pid {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        unsafe { Term::new(env, pid::make_pid(env.as_c_arg(), self.c)) }
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
    pub fn pid(self) -> Pid {
        let mut pid: ErlNifPid = unsafe { mem::uninitialized() };
        if unsafe { rustler_sys::enif_self(self.as_c_arg(), &mut pid) }.is_null() {
            panic!("environment is process-independent");
        }
        Pid { c: pid }
    }
}
