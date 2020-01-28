use crate::types::LocalPid;
use crate::wrapper::{NIF_ENV, NIF_TERM};
use crate::{Encoder, Term};
use std::marker::PhantomData;
use std::ptr;
use std::sync::{Arc, Weak};

/// Private type system hack to help ensure that each environment exposed to safe Rust code is
/// given a different lifetime. The size of this type is zero, so it costs nothing at run time. Its
/// purpose is to make `Env<'a>` and `Term<'a>` *invariant* w.r.t. `'a`, so that Rust won't
/// auto-convert a `Env<'a>` to a `Env<'b>`.
type EnvId<'a> = PhantomData<*mut &'a u8>;

/// On each NIF call, a Env is passed in. The Env is used for most operations that involve
/// communicating with the BEAM, like decoding and encoding terms.
///
/// There is no way to allocate a Env at the moment, but this may be possible in the future.
#[derive(Clone, Copy)]
pub struct Env<'a> {
    env: NIF_ENV,
    id: EnvId<'a>,
}

/// Two environments are equal if they're the same `NIF_ENV` value.
///
/// A `Env<'a>` is equal to a `Env<'b>` if and only if `'a` and `'b` are the same lifetime.
impl<'a, 'b> PartialEq<Env<'b>> for Env<'a> {
    fn eq(&self, other: &Env<'b>) -> bool {
        self.env == other.env
    }
}

impl<'a> Env<'a> {
    /// Create a new Env. For the `_lifetime_marker` argument, pass a
    /// reference to any local variable that has its own lifetime, different
    /// from all other `Env` values. The purpose of the argument is to make
    /// it easier to know for sure that the `Env` you're creating has a
    /// unique lifetime (i.e. that you're following the most important safety
    /// rule of Rustler).
    ///
    /// # Unsafe
    /// Don't create multiple `Env`s with the same lifetime.
    pub unsafe fn new<T>(_lifetime_marker: &'a T, env: NIF_ENV) -> Env<'a> {
        Env {
            env,
            id: PhantomData,
        }
    }

    pub fn as_c_arg(self) -> NIF_ENV {
        self.env
    }

    /// Convenience method for building a tuple `{error, Reason}`.
    pub fn error_tuple<T>(self, reason: T) -> Term<'a>
    where
        T: Encoder,
    {
        let error = crate::types::atom::error().to_term(self);
        (error, reason).encode(self)
    }

    /// Send a message to a process.
    ///
    /// The Erlang VM imposes some odd restrictions on sending messages.
    /// You can send messages in either of these situations:
    ///
    /// *   The current thread is managed by the Erlang VM, and `self` is the
    ///     environment of the calling process (that is, the environment that
    ///     Rustler passed in to your NIF); *or*
    ///
    /// *   The current thread is *not* managed by the Erlang VM.
    ///
    /// # Panics
    ///
    /// Panics if the above rules are broken (by trying to send a message from
    /// an `OwnedEnv` on a thread that's managed by the Erlang VM).
    ///
    pub fn send(self, pid: &LocalPid, message: Term<'a>) {
        let thread_type = unsafe { rustler_sys::enif_thread_type() };
        let env = if thread_type == rustler_sys::ERL_NIF_THR_UNDEFINED {
            ptr::null_mut()
        } else if thread_type == rustler_sys::ERL_NIF_THR_NORMAL_SCHEDULER
            || thread_type == rustler_sys::ERL_NIF_THR_DIRTY_CPU_SCHEDULER
            || thread_type == rustler_sys::ERL_NIF_THR_DIRTY_IO_SCHEDULER
        {
            // Panic if `self` is not the environment of the calling process.
            self.pid();

            self.as_c_arg()
        } else {
            panic!("Env::send(): unrecognized calling thread type");
        };

        // Send the message.
        unsafe {
            rustler_sys::enif_send(env, pid.as_c_arg(), ptr::null_mut(), message.as_c_arg());
        }
    }

    /// Decodes binary data to a term.
    ///
    /// Follows the erlang
    /// [External Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html).
    pub fn binary_to_term(self, data: &[u8]) -> Option<(Term<'a>, usize)> {
        unsafe {
            crate::wrapper::env::binary_to_term(self.as_c_arg(), data, true)
                .map(|(term, size)| (Term::new(self, term), size))
        }
    }

    /// Like `binary_to_term`, but can only be called on valid
    /// and trusted data.
    pub unsafe fn binary_to_term_trusted(self, data: &[u8]) -> Option<(Term<'a>, usize)> {
        crate::wrapper::env::binary_to_term(self.as_c_arg(), data, false)
            .map(|(term, size)| (Term::new(self, term), size))
    }
}

/// A process-independent environment, a place where Erlang terms can be created outside of a NIF
/// call.
///
/// Rust code can use an owned environment to build a message and send it to an
/// Erlang process.
///
///     use rustler::env::OwnedEnv;
///     use rustler::types::LocalPid;
///     use rustler::Encoder;
///
///     fn send_string_to_pid(data: &str, pid: &LocalPid) {
///         let mut msg_env = OwnedEnv::new();
///         msg_env.send_and_clear(pid, |env| data.encode(env));
///     }
///
/// There's no way to run Erlang code in an `OwnedEnv`. It's not a process. It's just a workspace
/// for building terms.
pub struct OwnedEnv {
    env: Arc<NIF_ENV>,
}

unsafe impl Send for OwnedEnv {}

impl OwnedEnv {
    /// Allocates a new process-independent environment.
    pub fn new() -> OwnedEnv {
        OwnedEnv {
            env: Arc::new(unsafe { rustler_sys::enif_alloc_env() }),
        }
    }

    /// Run some code in this environment.
    pub fn run<F, R>(&self, closure: F) -> R
    where
        F: for<'a> FnOnce(Env<'a>) -> R,
    {
        let env = unsafe { Env::new(&(), *self.env) };
        closure(env)
    }

    /// Send a message from a Rust thread to an Erlang process.
    ///
    /// The environment is cleared as though by calling the `.clear()` method.
    /// To avoid that, use `env.send(pid, term)` instead.
    ///
    /// # Panics
    ///
    /// Panics if called from a thread that is managed by the Erlang VM. You
    /// can only use this method on a thread that was created by other
    /// means. (This curious restriction is imposed by the Erlang VM.)
    ///
    pub fn send_and_clear<F>(&mut self, recipient: &LocalPid, closure: F)
    where
        F: for<'a> FnOnce(Env<'a>) -> Term<'a>,
    {
        if unsafe { rustler_sys::enif_thread_type() } != rustler_sys::ERL_NIF_THR_UNDEFINED {
            panic!("send_and_clear: current thread is managed");
        }

        let message = self.run(|env| closure(env).as_c_arg());

        unsafe {
            rustler_sys::enif_send(ptr::null_mut(), recipient.as_c_arg(), *self.env, message);
        }

        self.clear();
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
        unsafe {
            rustler_sys::enif_clear_env(c_env);
        }
    }

    /// Save a term for use in a later call to `.run()` or `.send()`.
    ///
    /// For your safety, Rust doesn't let you save `Term` values from one `.run()` call to a
    /// later `.run()` call. If you try, it'll complain about lifetimes.
    ///
    /// `.save()` offers a way to do this. For example, maybe you'd like to copy a term from the
    /// caller into an `OwnedEnv`, then use that term on another thread.
    ///
    ///     # use rustler::{ Env, Term };
    ///     use rustler::env::OwnedEnv;
    ///     use std::thread;
    ///
    ///     fn thread_example<'a>(env: Env<'a>, term: Term<'a>) {
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
    pub fn save<'a>(&self, term: Term<'a>) -> SavedTerm {
        SavedTerm {
            term: self.run(|env| term.in_env(env).as_c_arg()),
            env_generation: Arc::downgrade(&self.env),
        }
    }
}

impl Drop for OwnedEnv {
    fn drop(&mut self) {
        unsafe {
            rustler_sys::enif_free_env(*self.env);
        }
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
    /// Load this saved term back into its environment.
    ///
    /// # Panics
    ///
    /// `env` must be the `Env` of a `.run()` or `.send()` call on the
    /// `OwnedEnv` where this term was saved, and the `OwnedEnv` must not have
    /// been cleared or dropped since then. Otherwise this method will panic.
    pub fn load<'a>(&self, env: Env<'a>) -> Term<'a> {
        // Check that the saved term is still valid.
        match self.env_generation.upgrade() {
            None => panic!("term is from a cleared or dropped OwnedEnv"),
            Some(ref env_arc) if **env_arc == env.as_c_arg() => unsafe {
                Term::new(env, self.term)
            },
            _ => panic!("can't load SavedTerm into a different environment"),
        }
    }
}

impl Default for OwnedEnv {
    fn default() -> Self {
        Self::new()
    }
}
