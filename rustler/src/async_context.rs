use std::cell::{Cell, RefCell};
use std::sync::Arc;

pub(crate) type KeepAlive = Arc<dyn Send + Sync>;

#[derive(Clone, Copy, Default)]
pub(crate) struct Suspension {
    pub yielded: bool,
    pub executor: bool,
    pub executor_enabled: bool,
}

thread_local! {
    static CURRENT: Cell<Option<Suspension>> = const { Cell::new(None) };
    static KEEP_ALIVE: RefCell<Option<KeepAlive>> = const { RefCell::new(None) };
}

pub(crate) struct PollScope {
    previous: Option<Suspension>,
    keep_alive: Option<KeepAlive>,
}

impl PollScope {
    pub fn enter(executor_enabled: bool, keep_alive: Option<KeepAlive>) -> Self {
        Self {
            previous: CURRENT.replace(Some(Suspension {
                executor_enabled,
                ..Suspension::default()
            })),
            keep_alive: KEEP_ALIVE.replace(keep_alive),
        }
    }

    pub fn suspension(&self) -> Suspension {
        CURRENT.get().expect("missing async NIF poll scope")
    }
}

impl Drop for PollScope {
    fn drop(&mut self) {
        CURRENT.set(self.previous);
        KEEP_ALIVE.replace(self.keep_alive.take());
    }
}

pub(crate) fn request_yield() {
    if let Some(mut current) = CURRENT.get() {
        current.yielded = true;
        CURRENT.set(Some(current));
    }
}

pub(crate) fn executor_enabled() -> bool {
    CURRENT
        .get()
        .is_some_and(|current| current.executor_enabled)
}

pub(crate) fn executor_keep_alive() -> Option<KeepAlive> {
    KEEP_ALIVE.with_borrow(Clone::clone)
}

pub(crate) fn request_executor_wait() {
    if let Some(mut current) = CURRENT.get() {
        current.executor = true;
        CURRENT.set(Some(current));
    }
}
