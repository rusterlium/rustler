use ::{ NifEnv, NifTerm };
use wrapper::nif_interface::{ self, NIF_ENV, ErlNifPid };
use std::mem;

pub struct OwnedEnv {
    env: NIF_ENV
}

unsafe impl Send for OwnedEnv {}

impl OwnedEnv {
    pub fn new() -> OwnedEnv {
        OwnedEnv {
            env: unsafe { nif_interface::enif_alloc_env() }
        }
    }

    /// Run some code in this environment.
    pub fn run<F, R>(&self, closure: F) -> R
        where F: for<'a> FnOnce(NifEnv<'a>) -> R
    {
        let env_lifetime = ();
        let env = unsafe { NifEnv::new(&env_lifetime, self.env) };
        closure(env)
    }

    /// Run a closure that produces a term, then send the term to another process.
    ///
    /// After the closure runs and the message is sent, the environment is cleared as though by
    /// calling the `.clear()` method.
    pub fn send<F>(&mut self, recipient: ErlNifPid, closure: F)
        where F: for<'a> FnOnce(NifEnv<'a>) -> NifTerm<'a>
    {
        let env_lifetime = ();
        let env = unsafe { NifEnv::new(&env_lifetime, self.env) };
        let message = closure(env);
        unsafe {
            nif_interface::enif_send(self.env, &recipient, self.env, message.as_c_arg());
        }
    }

    /// Free all terms in this environment and clear it for reuse.
    ///
    /// Unless you call this method after a call to `run()`, all terms created within the
    /// environment hang around in memory until the `OwnedEnv` is dropped: garbage collection does
    /// not continually happen as needed in a NIF environment.
    pub fn clear(&mut self) {
        unsafe { nif_interface::enif_clear_env(self.env); }
    }
}

impl Drop for OwnedEnv {
    fn drop(&mut self) {
        unsafe { nif_interface::enif_free_env(self.env); }
    }
}
