use std::ffi::CString;
use std::future::Future;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Wake};

use super::NifReturned;
use crate::env::SavedTerm;
use crate::sys::{enif_make_copy, ErlNifEnv, ErlNifTerm};
use crate::{
    Encoder, Env, LocalPid, Monitor, Nif, OwnedEnv, ProcessIndependent, Resource, ResourceArc,
    SchedulerFlags, Term,
};

mod atoms {
    crate::atoms! {
        rustler_async_wait,
        rustler_async_ready,
        rustler_async_done,
        async_nif_pending_without_yield,
    }
}

pub type AsyncNifFuture<'a> = Pin<Box<dyn Future<Output = NifReturned> + Send + 'a>>;
pub type AsyncNifFactory =
    for<'a> fn(Env<'a, ProcessIndependent>, Vec<Term<'a>>) -> AsyncNifFuture<'a>;

pub struct AsyncNifRegistration {
    pub executor: bool,
}

inventory::collect!(AsyncNifRegistration);

#[derive(Clone, Copy, PartialEq)]
enum Phase {
    Polling,
    Waiting,
    Notified,
    Cancelled,
}

struct Notification {
    pid: LocalPid,
    env: OwnedEnv,
    reference: SavedTerm,
}

struct WakeState {
    phase: Phase,
    woken: bool,
    notification: Option<Notification>,
}

struct AsyncWake(Mutex<WakeState>);

impl AsyncWake {
    fn cancel(&self) {
        self.0.lock().unwrap().phase = Phase::Cancelled;
    }

    fn begin_poll(&self) {
        let mut state = self.0.lock().unwrap();
        state.phase = Phase::Polling;
        state.woken = false;
    }

    fn wait(&self) -> bool {
        let mut state = self.0.lock().unwrap();
        if state.woken {
            false
        } else {
            state.phase = Phase::Waiting;
            true
        }
    }

    fn reference<'a>(&self, env: Env<'a>) -> Term<'a> {
        let state = self.0.lock().unwrap();
        let notification = state.notification.as_ref().unwrap();
        notification
            .env
            .run(|saved_env| notification.reference.load(saved_env).in_env(env))
    }
}

impl Wake for AsyncWake {
    fn wake(self: Arc<Self>) {
        self.wake_by_ref();
    }

    fn wake_by_ref(self: &Arc<Self>) {
        let message = {
            let mut state = self.0.lock().unwrap();
            if state.phase == Phase::Cancelled {
                return;
            }
            state.woken = true;
            if state.phase != Phase::Waiting {
                return;
            }
            state.phase = Phase::Notified;
            let notification = state.notification.as_ref().unwrap();
            let env = OwnedEnv::new();
            let message = notification.env.run(|saved_env| {
                env.save((
                    atoms::rustler_async_ready(),
                    notification.reference.load(saved_env),
                ))
            });
            (notification.pid, env, message)
        };
        let wake = self.clone();
        let notify = move || {
            let (pid, mut env, message) = message;
            if env.send_and_clear(&pid, |env| message.load(env)).is_err() {
                wake.cancel();
            }
        };
        // Executor adapters can wake a task from any thread, including another NIF.
        // Process-independent sends must run on a non-scheduler thread.
        if crate::thread::is_scheduler_thread() {
            std::thread::spawn(notify);
        } else {
            notify();
        }
    }
}

struct AsyncState {
    // Field order ensures the future and its borrowed data are dropped before the env.
    future: Option<AsyncNifFuture<'static>>,
    env: Option<OwnedEnv>,
    name: &'static str,
    flags: SchedulerFlags,
    wake: Arc<AsyncWake>,
    monitor: Option<Monitor>,
    executor: bool,
}

struct AsyncNif {
    state: Mutex<AsyncState>,
}

impl Resource for AsyncNif {
    const IMPLEMENTS_DOWN: bool = true;

    fn down<'a>(&'a self, _env: Env<'a>, _pid: LocalPid, _monitor: Monitor) {
        let mut state = self.state.lock().unwrap();
        state.wake.cancel();
        if drop_future(&mut state).is_err() {
            // Resource callbacks cannot return an exception to a dead process.
            eprintln!("async NIF future panicked during process termination");
        }
    }
}

impl Drop for AsyncNif {
    fn drop(&mut self) {
        let state = self
            .state
            .get_mut()
            .unwrap_or_else(|error| error.into_inner());
        state.wake.cancel();
        if drop_future(state).is_err() {
            eprintln!("async NIF future panicked during resource cleanup");
        }
    }
}

pub fn async_nif_function() -> Option<Nif> {
    inventory::iter::<AsyncNifRegistration>()
        .any(|registration| registration.executor)
        .then_some(Nif {
            name: c"__rustler_async_continue".as_ptr(),
            arity: 1,
            flags: SchedulerFlags::Normal as u32,
            raw_func: continue_nif,
        })
}

pub fn register_async_nif(env: Env<'_>) -> Result<(), crate::ResourceInitError> {
    if inventory::iter::<AsyncNifRegistration>().next().is_some() {
        env.register::<AsyncNif>()?;
    }
    Ok(())
}

pub fn start_async_nif(
    env: Env<'_>,
    args: &[Term<'_>],
    name: &'static str,
    flags: SchedulerFlags,
    executor: bool,
    factory: AsyncNifFactory,
) -> NifReturned {
    let result = catch_unwind(AssertUnwindSafe(|| {
        let mut owned = OwnedEnv::new();
        let handle = owned.borrow();
        let local = handle.as_env();
        let args = args.iter().map(|term| term.in_env(local)).collect();
        let future = factory(handle, args);

        // The higher-ranked factory cannot borrow the caller's environment or stack.
        // Its only borrowed input is owned's stable allocation. The resource retains
        // that allocation and always drops the future first, including on cancellation.
        let future: AsyncNifFuture<'static> = unsafe { std::mem::transmute(future) };
        let notification = executor.then(|| {
            let notification_env = OwnedEnv::new();
            let reference = notification_env.save(env.make_ref());
            Notification {
                pid: env.pid(),
                env: notification_env,
                reference,
            }
        });
        let resource = ResourceArc::new(AsyncNif {
            state: Mutex::new(AsyncState {
                future: Some(future),
                env: Some(owned),
                name,
                flags,
                wake: Arc::new(AsyncWake(Mutex::new(WakeState {
                    phase: Phase::Polling,
                    woken: false,
                    notification,
                }))),
                monitor: None,
                executor,
            }),
        });
        if executor {
            let monitor = resource
                .monitor(Some(env), &env.pid())
                .expect("could not monitor async NIF caller");
            resource.state.lock().unwrap().monitor = Some(monitor);
        }
        poll(&resource, env)
    }));
    finish(result)
}

fn poll(resource: &ResourceArc<AsyncNif>, env: Env<'_>) -> NifReturned {
    let mut state = resource.state.lock().unwrap();
    state.wake.begin_poll();
    let waker = std::task::Waker::from(state.wake.clone());
    let mut context = Context::from_waker(&waker);
    let keep_alive = state
        .executor
        .then(|| Arc::new(resource.clone()) as crate::async_context::KeepAlive);
    let scope = crate::async_context::PollScope::enter(state.executor, keep_alive);
    let polled = catch_unwind(AssertUnwindSafe(|| {
        state
            .future
            .as_mut()
            .expect("async NIF already completed")
            .as_mut()
            .poll(&mut context)
    }));
    let suspension = scope.suspension();
    drop(scope);
    match polled {
        Ok(Poll::Ready(returned)) => {
            let returned = match returned {
                NifReturned::Term(term) => {
                    NifReturned::Term(unsafe { enif_make_copy(env.as_c_arg(), term) })
                }
                NifReturned::Raise(term) => {
                    NifReturned::Raise(unsafe { enif_make_copy(env.as_c_arg(), term) })
                }
                NifReturned::Reschedule {
                    fun_name,
                    flags,
                    fun,
                    args,
                } => NifReturned::Reschedule {
                    fun_name,
                    flags,
                    fun,
                    args: args
                        .into_iter()
                        .map(|term| unsafe { enif_make_copy(env.as_c_arg(), term) })
                        .collect(),
                },
                other => other,
            };
            let returned = match returned {
                NifReturned::Term(term) if state.executor => NifReturned::Term(
                    (atoms::rustler_async_done(), unsafe { Term::new(env, term) })
                        .encode(env)
                        .as_c_arg(),
                ),
                other => other,
            };
            complete(resource, &mut state, env);
            match drop_future(&mut state) {
                Ok(()) => returned,
                Err(panic) => finish(Err(panic)),
            }
        }
        Ok(Poll::Pending) if suspension.yielded => reschedule(resource, &state, env),
        Ok(Poll::Pending) if suspension.executor && state.executor => {
            if state.wake.wait() {
                NifReturned::Term(
                    (
                        atoms::rustler_async_wait(),
                        state.wake.reference(env),
                        resource,
                    )
                        .encode(env)
                        .as_c_arg(),
                )
            } else {
                reschedule(resource, &state, env)
            }
        }
        Ok(Poll::Pending) => {
            complete(resource, &mut state, env);
            match drop_future(&mut state) {
                Ok(()) => NifReturned::Raise(atoms::async_nif_pending_without_yield().as_c_arg()),
                Err(panic) => finish(Err(panic)),
            }
        }
        Err(panic) => {
            // Drop outside the poll's unwinding: a destructor panic must not cause
            // a double panic while cleaning up a failed poll.
            complete(resource, &mut state, env);
            match drop_future(&mut state) {
                Ok(()) => finish(Err(panic)),
                Err(drop_panic) => finish(Err(drop_panic)),
            }
        }
    }
}

fn reschedule(resource: &ResourceArc<AsyncNif>, state: &AsyncState, env: Env<'_>) -> NifReturned {
    crate::schedule::consume_timeslice(env, 100);
    NifReturned::Reschedule {
        fun_name: CString::new(state.name).unwrap(),
        flags: state.flags,
        fun: resume,
        args: vec![resource.encode(env).as_c_arg()],
    }
}

fn complete(resource: &ResourceArc<AsyncNif>, state: &mut AsyncState, env: Env<'_>) {
    state.wake.cancel();
    if let Some(monitor) = state.monitor.take() {
        resource.demonitor(Some(env), &monitor);
    }
}

fn drop_future(state: &mut AsyncState) -> std::thread::Result<()> {
    let future = state.future.take();
    let result = catch_unwind(AssertUnwindSafe(|| drop(future)));
    drop(state.env.take());
    result
}

fn finish(result: std::thread::Result<NifReturned>) -> NifReturned {
    match result {
        Ok(returned) => returned,
        Err(_) => NifReturned::Raise(crate::types::atom::nif_panicked().as_c_arg()),
    }
}

unsafe extern "C" fn resume(
    raw_env: *mut ErlNifEnv,
    argc: i32,
    argv: *const ErlNifTerm,
) -> ErlNifTerm {
    let lifetime = ();
    let env = Env::new(&lifetime, raw_env);
    let result = catch_unwind(AssertUnwindSafe(|| {
        if argc != 1 {
            return NifReturned::BadArg;
        }

        let term = Term::new(env, *argv);
        match term.decode::<ResourceArc<AsyncNif>>() {
            Ok(resource) => poll(&resource, env),
            Err(_) => NifReturned::BadArg,
        }
    }));
    finish(result).apply(env)
}

unsafe extern "C" fn continue_nif(
    raw_env: *mut ErlNifEnv,
    argc: i32,
    argv: *const ErlNifTerm,
) -> ErlNifTerm {
    let lifetime = ();
    let env = Env::new(&lifetime, raw_env);
    let result = catch_unwind(AssertUnwindSafe(|| {
        if argc != 1 {
            return NifReturned::BadArg;
        }
        let resource = match Term::new(env, *argv).decode::<ResourceArc<AsyncNif>>() {
            Ok(resource) => resource,
            Err(_) => return NifReturned::BadArg,
        };
        let state = resource.state.lock().unwrap();
        let wake = state.wake.0.lock().unwrap();
        let valid = wake.phase == Phase::Notified
            && wake
                .notification
                .as_ref()
                .is_some_and(|notification| notification.pid == env.pid());
        drop(wake);
        if valid {
            reschedule(&resource, &state, env)
        } else {
            NifReturned::BadArg
        }
    }));
    finish(result).apply(env)
}
