use std::sync::atomic::{AtomicU64, Ordering};

static TASK_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Task reference resource for async tasks.
///
/// This is automatically created by `#[rustler::task]` and returned to the caller.
/// All messages sent by the task (both intermediate and final) are tagged with this reference.
#[cfg(feature = "tokio_rt")]
#[derive(Debug, Clone)]
pub struct TaskRef {
    #[allow(dead_code)]
    id: u64,
}

#[cfg(feature = "tokio_rt")]
impl TaskRef {
    /// Create a new TaskRef with a unique ID.
    ///
    /// This is used internally by the `#[rustler::task]` macro.
    #[doc(hidden)]
    pub fn new() -> Self {
        Self {
            id: TASK_COUNTER.fetch_add(1, Ordering::Relaxed),
        }
    }
}

// Implement Resource trait
#[cfg(feature = "tokio_rt")]
impl crate::Resource for TaskRef {}

// Auto-register TaskRef resource via inventory
#[cfg(feature = "tokio_rt")]
crate::codegen_runtime::inventory::submit! {
    crate::resource::Registration::new::<TaskRef>()
}
