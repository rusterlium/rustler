use ::{ NifEnv, NifTerm };
use wrapper::copy_term;
use wrapper::nif_interface::{ self, NIF_ENV, NIF_TERM };
pub use wrapper::nif_interface::ErlNifPid;
use std::mem;
use std::sync::{Arc, Weak};

impl<'a> NifEnv<'a> {
    /// Return the calling process's pid.
    ///
    /// # Panics
    ///
    /// Panics if this environment is process-independent.  (The only way to get such an
    /// environment is to use `OwnedEnv`.  The `NifEnv` that Rustler passes to NIFs when they're
    /// called is always associated with the calling Erlang process.)
    pub fn pid(self) -> ErlNifPid {
        let mut pid: ErlNifPid = unsafe { mem::uninitialized() };
        if unsafe { nif_interface::enif_self(self.as_c_arg(), &mut pid) }.is_null() {
            panic!("environment is process-independent");
        }
        pid
    }
}


// Helper function used by save() and load().
unsafe fn nif_term_into_env<'a>(term: NIF_TERM, src_env: NIF_ENV, dest_env: NIF_ENV) -> NIF_TERM {
    if src_env == dest_env {
        term
    } else {
        copy_term(dest_env, term)
    }
}


/// A process-independent environment.
///
/// An owned environment is a place where Erlang terms can be created outside of a NIF call. Rust
/// code can use an owned environment to build a message and send it to an Erlang process.
///
///     use rustler::env::{OwnedEnv, ErlNifPid};
///     use rustler::NifEncoder;
///
///     fn send_string_to_pid(data: &str, pid: ErlNifPid) {
///         let mut msg_env = OwnedEnv::new();
///         msg_env.send(pid, |env| data.encode(env));
///     }
///
/// There's no way to run Erlang code in an owned environment. It's not a process. It's just a
/// workspace for building terms.
pub struct OwnedEnv {
    env: Arc<NIF_ENV>
}

unsafe impl Send for OwnedEnv {}

impl OwnedEnv {
    /// Allocates a new process-independent environment.
    pub fn new() -> OwnedEnv {
        OwnedEnv {
            env: Arc::new(unsafe { nif_interface::enif_alloc_env() })
        }
    }

    /// Run some code in this environment.
    pub fn run<F, R>(&self, closure: F) -> R
        where F: for<'a> FnOnce(NifEnv<'a>) -> R
    {
        let env_lifetime = ();
        let env = unsafe { NifEnv::new(&env_lifetime, *self.env) };
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
        let c_env = *self.env;
        let env = unsafe { NifEnv::new(&env_lifetime, c_env) };
        let message = closure(env);

        self.env = Arc::new(c_env);
        unsafe {
            nif_interface::enif_send(*self.env, &recipient, *self.env, message.as_c_arg());
        }
    }

    /// Free all terms in this environment and clear it for reuse.
    ///
    /// This invalidates `SavedTerm`s that were saved in this environment;
    /// if you later try to `.load()` one, you'll get a panic.
    ///
    /// Unless you call this method after a call to `run()`, all terms created within the
    /// environment hang around in memory until the `OwnedEnv` is dropped: garbage collection does
    /// not continually happen as needed in a NIF environment.
    pub fn clear(&mut self) {
        let c_env = *self.env;
        self.env = Arc::new(c_env);
        unsafe { nif_interface::enif_clear_env(c_env); }
    }

    /// Save a term for use in a later call to `.run()` or `.send()`.
    ///
    /// For your safety, Rust doesn't let you save `NifTerm` values from one `.run()` call to a
    /// later `.run()` call. If you try, it'll complain about lifetimes.
    ///
    /// `.save()` offers a way to do this. For example, maybe you'd like to copy a term from the
    /// caller into an `OwnedEnv`, then use that term on another thread.
    ///
    ///     # use rustler::{ NifEnv, NifTerm };
    ///     use rustler::env::OwnedEnv;
    ///     use std::thread;
    ///
    ///     fn thread_example<'a>(env: NifEnv<'a>, term: NifTerm<'a>) {
    ///         // Copy `term` into a new OwnedEnv, for use on another thread.
    ///         let mut thread_env = OwnedEnv::new();
    ///         let saved_term = thread_env.save(term);
    ///
    ///         thread::spawn(move || {
    ///             // Now run some code on the thread, using the saved term.
    ///             thread_env.run(|env| {
    ///                 let term = saved_term.load(env);
    ///                 //... do stuff with term ...
    ///             });
    ///         });
    ///     }
    ///
    /// **Note: There is no way to save terms across `OwnedEnv::send()` or `clear()`.**
    /// If you try, the `.load()` call will panic.
    pub fn save<'a>(&self, term: NifTerm<'a>) -> SavedTerm {
        SavedTerm {
            term: unsafe { nif_term_into_env(term.as_c_arg(), term.get_env().as_c_arg(), *self.env) },
            env_generation: Arc::downgrade(&self.env),
        }
    }
}

impl Drop for OwnedEnv {
    fn drop(&mut self) {
        unsafe { nif_interface::enif_free_env(*self.env); }
    }
}

/// A term that was created in an `OwnedEnv` and saved for later use.
///
/// These are created by calling `OwnedEnv::save()`. See that method's documentation for an
/// example.
#[derive(Clone)]
pub struct SavedTerm {
    env_generation: Weak<NIF_ENV>,
    term: NIF_TERM,
}

unsafe impl Send for SavedTerm {}

impl SavedTerm {
    /// Load this saved term into any `NifEnv`.
    ///
    /// If `env` isn't in the same `OwnedEnv` where this term was saved, the term will be copied.
    ///
    /// # Panics
    /// Panics if the `OwnedEnv` where this term was saved has been cleared or dropped.
    pub fn load<'a>(&self, env: NifEnv<'a>) -> NifTerm<'a> {
        // Check that the saved term is still valid.
        match self.env_generation.upgrade() {
            None =>
                panic!("term is from a cleared or dropped OwnedEnv"),
            Some(env_arc) =>
                unsafe { NifTerm::new(env, nif_term_into_env(self.term, *env_arc, env.as_c_arg())) }
        }
    }
}
