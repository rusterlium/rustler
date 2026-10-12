use crate::sys::enif_consume_timeslice;
use crate::Env;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

#[derive(Clone, Copy, Debug)]
pub enum SchedulerFlags {
    Normal = crate::sys::ERL_NIF_NORMAL_JOB as isize,
    DirtyCpu = crate::sys::ERL_NIF_DIRTY_JOB_CPU_BOUND as isize,
    DirtyIo = crate::sys::ERL_NIF_DIRTY_JOB_IO_BOUND as isize,
}

pub fn consume_timeslice(env: Env, percent: i32) -> bool {
    let success = unsafe { enif_consume_timeslice(env.as_c_arg(), percent) };
    success == 1
}

/// Yield an async NIF to the BEAM scheduler.
///
/// The first poll requests rescheduling through `enif_schedule_nif`; the next
/// poll completes. No external async executor is required.
pub fn yield_now() -> YieldNow {
    YieldNow(false)
}

/// Future returned by [`yield_now`].
#[must_use = "yielding requires awaiting this future"]
pub struct YieldNow(bool);

impl Future for YieldNow {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<()> {
        if self.0 {
            Poll::Ready(())
        } else {
            self.0 = true;
            crate::async_context::request_yield();
            context.waker().wake_by_ref();
            Poll::Pending
        }
    }
}
