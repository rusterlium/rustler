use crate::sys::{enif_alloc_env, enif_clear_env, enif_free_env, enif_send, enif_whereis_pid};
use crate::thread::is_scheduler_thread;
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum EnvKind {
    ProcessBound,
    Callback,
    Init,
    ProcessIndependent,
}

/// On each NIF call, a Env is passed in. The Env is used for most operations that involve
/// communicating with the BEAM, like decoding and encoding terms.
///
/// There is no way to allocate a Env at the moment, but this may be possible in the future.
#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub(crate) kind: EnvKind,
    env: NIF_ENV,
    id: EnvId<'a>,
}

/// Two environments are equal if they're the same `NIF_ENV` value.
///
/// A `Env<'a>` is equal to a `Env<'b>` if and only if `'a` and `'b` are the same lifetime.
impl<'b> PartialEq<Env<'b>> for Env<'_> {
    fn eq(&self, other: &Env<'b>) -> bool {
        self.env == other.env
    }
}

/// Indicates that a send failed, see
/// [enif\_send](https://www.erlang.org/doc/man/erl_nif.html#enif_send).
#[derive(Clone, Copy, Debug)]
pub struct SendError;

impl<'a> Env<'a> {
    #[doc(hidden)]
    #[inline]
    pub(crate) unsafe fn new_internal<T>(
        _lifetime_marker: &'a T,
        env: NIF_ENV,
        kind: EnvKind,
    ) -> Env<'a> {
        Env {
            kind,
            env,
            id: PhantomData,
        }
    }

    /// Create a new Env. For the `_lifetime_marker` argument, pass a
    /// reference to any local variable that has its own lifetime, different
    /// from all other `Env` values. The purpose of the argument is to make
    /// it easier to know for sure that the `Env` you're creating has a
    /// unique lifetime (i.e. that you're following the most important safety
    /// rule of Rustler).
    ///
    /// # Unsafe
    /// Don't create multiple `Env`s with the same lifetime.
    #[inline]
    pub unsafe fn new<T>(_lifetime_marker: &'a T, env: NIF_ENV) -> Env<'a> {
        Self::new_internal(_lifetime_marker, env, EnvKind::ProcessBound)
    }

    #[doc(hidden)]
    #[inline]
    pub unsafe fn new_init_env<T>(_lifetime_marker: &'a T, env: NIF_ENV) -> Env<'a> {
        Self::new_internal(_lifetime_marker, env, EnvKind::Init)
    }

    pub fn as_c_arg(self) -> NIF_ENV {
        self.env
    }

    /// Convenience method for building a tuple `{error, Reason}`.
    #[inline]
    pub fn error_tuple(self, reason: impl Encoder) -> Term<'a> {
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
    /// The result indicates whether the send was successful, see also
    /// [enif\_send](https://www.erlang.org/doc/man/erl_nif.html#enif_send).
    #[inline]
    pub fn send(self, pid: &LocalPid, message: impl Encoder) -> Result<(), SendError> {
        let env = if is_scheduler_thread() {
            if self.kind == EnvKind::ProcessIndependent {
                return Err(SendError);
            }

            self.as_c_arg()
        } else {
            ptr::null_mut()
        };

        let message = message.encode(self);

        // Send the message.
        let res = unsafe { enif_send(env, pid.as_c_arg(), ptr::null_mut(), message.as_c_arg()) };

        if res == 1 {
            Ok(())
        } else {
            Err(SendError)
        }
    }

    /// Attempts to find the PID of a process registered by `name_or_pid`
    ///
    /// Safe wrapper around [`enif_whereis_pid`](https://www.erlang.org/doc/man/erl_nif.html#enif_whereis_pid).
    ///
    /// # Returns
    /// - `Some(pid)` if `name_or_pid` is already a PID.
    /// - `Some(pid)` if `name_or_pid` is an atom and an alive process is currently registered under the given name.
    /// - `None` if `name_or_pid` is an atom but there is no alive process registered under this name.
    /// - `None` if `name_or_pid` is not a PID or atom.
    pub fn whereis_pid(self, name_or_pid: impl Encoder) -> Option<LocalPid> {
        let name_or_pid = name_or_pid.encode(self);
        if name_or_pid.is_pid() {
            return Some(name_or_pid.decode().unwrap());
        }

        let mut enif_pid = std::mem::MaybeUninit::uninit();

        if unsafe {
            enif_whereis_pid(
                self.as_c_arg(),
                name_or_pid.as_c_arg(),
                enif_pid.as_mut_ptr(),
            )
        } == 0
        {
            // If `name_or_pid` is not an atom, or not the name of a registered process
            None
        } else {
            // Safety: Initialized by successful enif_whereis_pid call
            let enif_pid = unsafe { enif_pid.assume_init() };

            let pid = LocalPid::from_c_arg(enif_pid);
            Some(pid)
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
///         let _ = msg_env.send_and_clear(pid, |env| data.encode(env));
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
    #[allow(clippy::arc_with_non_send_sync)] // Likely false negative, see https://github.com/rust-lang/rust-clippy/issues/11382
    pub fn new() -> OwnedEnv {
        OwnedEnv {
            env: Arc::new(unsafe { enif_alloc_env() }),
        }
    }

    /// Run some code in this environment.
    pub fn run<'a, F, R>(&self, closure: F) -> R
    where
        F: FnOnce(Env<'a>) -> R,
    {
        let env = unsafe { Env::new_internal(&(), *self.env, EnvKind::ProcessIndependent) };
        closure(env)
    }

    /// Send a message from a Rust thread to an Erlang process.
    ///
    /// The environment is cleared as though by calling the `.clear()` method.
    /// To avoid that, use `env.send(pid, term)` instead.
    ///
    /// The result is the same as what `Env::send` would return.
    ///
    /// # Panics
    ///
    /// Panics if called from a thread that is managed by the Erlang VM. You
    /// can only use this method on a thread that was created by other
    /// means. (This curious restriction is imposed by the Erlang VM.)
    ///
    pub fn send_and_clear<'a, F, T>(
        &mut self,
        recipient: &LocalPid,
        closure: F,
    ) -> Result<(), SendError>
    where
        F: FnOnce(Env<'a>) -> T,
        T: Encoder,
    {
        if is_scheduler_thread() {
            panic!("send_and_clear: current thread is managed");
        }

        let message = self.run(|env| closure(env).encode(env).as_c_arg());

        let res = unsafe { enif_send(ptr::null_mut(), recipient.as_c_arg(), *self.env, message) };

        self.clear();

        if res == 1 {
            Ok(())
        } else {
            Err(SendError)
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
    #[allow(clippy::arc_with_non_send_sync)] // Likely false negative, see https://github.com/rust-lang/rust-clippy/issues/11382
    pub fn clear(&mut self) {
        let c_env = *self.env;
        self.env = Arc::new(c_env);
        unsafe {
            enif_clear_env(c_env);
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
    pub fn save(&self, term: impl Encoder) -> SavedTerm {
        SavedTerm {
            term: self.run(|env| term.encode(env).as_c_arg()),
            env_generation: Arc::downgrade(&self.env),
        }
    }
}

impl Drop for OwnedEnv {
    fn drop(&mut self) {
        unsafe {
            enif_free_env(*self.env);
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
