use std::ffi::c_int;

use crate::sys::{
    enif_select, ErlNifEvent, ErlNifSelectFlags, ERL_NIF_SELECT_ERROR_CANCELLED,
    ERL_NIF_SELECT_FAILED, ERL_NIF_SELECT_INVALID_EVENT, ERL_NIF_SELECT_NOTSUP,
    ERL_NIF_SELECT_READ_CANCELLED, ERL_NIF_SELECT_STOP_CALLED, ERL_NIF_SELECT_STOP_SCHEDULED,
    ERL_NIF_SELECT_WRITE_CANCELLED,
};
use crate::types::atom::undefined;
use crate::{Encoder, Env, LocalPid, Reference, Resource, ResourceArc};

macro_rules! getter {
    (pub $name:ident, $flag:ident) => {
        #[inline]
        pub fn $name(self) -> bool {
            self.0 & $flag != 0
        }
    };
    ($name:ident, $flag:ident) => {
        #[inline]
        fn $name(self) -> bool {
            self.0 & $flag != 0
        }
    };
}

#[derive(Clone, Copy, Debug)]
pub struct SelectReturn(c_int);

pub type SelectResult = Result<SelectReturn, SelectError>;

impl From<SelectReturn> for SelectResult {
    fn from(val: SelectReturn) -> Self {
        use SelectError::*;

        if val.0 < 0 {
            if val.invalid_event() {
                Err(InvalidEvent)
            } else if val.failed() {
                Err(Failed)
            } else if val.not_supported() {
                Err(NotSupported)
            } else {
                Err(Unknown)
            }
        } else {
            Ok(val)
        }
    }
}

impl SelectReturn {
    pub fn cancelled(self) -> bool {
        self.read_cancelled() || self.write_cancelled() || self.error_cancelled()
    }

    getter! {pub stop_called, ERL_NIF_SELECT_STOP_CALLED}
    getter! {pub stop_scheduled, ERL_NIF_SELECT_STOP_SCHEDULED}
    getter! {pub read_cancelled, ERL_NIF_SELECT_READ_CANCELLED}
    getter! {pub write_cancelled, ERL_NIF_SELECT_WRITE_CANCELLED}
    getter! {pub error_cancelled, ERL_NIF_SELECT_ERROR_CANCELLED}
    getter! {invalid_event, ERL_NIF_SELECT_INVALID_EVENT}
    getter! {failed, ERL_NIF_SELECT_FAILED}
    getter! {not_supported, ERL_NIF_SELECT_NOTSUP}
}

#[derive(Clone, Copy, Debug)]
pub enum SelectError {
    InvalidEvent,
    Failed,
    NotSupported,
    Unknown,
}

#[derive(Clone, Copy, Debug)]
pub struct Event(ErlNifEvent);

impl From<ErlNifEvent> for Event {
    fn from(evt: ErlNifEvent) -> Self {
        Self(evt)
    }
}

impl From<Event> for ErlNifEvent {
    fn from(val: Event) -> Self {
        val.0
    }
}

#[derive(Clone, Copy, Debug)]
pub enum SelectMode {
    Read,
    Write,
    ReadWrite,
}

impl SelectMode {
    fn to_flags(self) -> c_int {
        use ErlNifSelectFlags::*;

        match self {
            SelectMode::Read => ERL_NIF_SELECT_READ as c_int,
            SelectMode::Write => ERL_NIF_SELECT_WRITE as c_int,
            SelectMode::ReadWrite => ERL_NIF_SELECT_READ as c_int | ERL_NIF_SELECT_WRITE as c_int,
        }
    }
}

impl<T> ResourceArc<T>
where
    T: Resource,
{
    pub fn select<'a>(
        &self,
        env: Env<'a>,
        event: Event,
        mode: SelectMode,
        pid: Option<LocalPid>,
        reference: Option<Reference>,
    ) -> SelectResult {
        let reference = match reference {
            Some(reference) => reference.encode(env),
            None => undefined().encode(env),
        }
        .as_c_arg();

        let pid = pid.map_or(std::ptr::null(), |p| p.as_c_arg());

        let res = unsafe {
            enif_select(
                env.as_c_arg(),
                event.0,
                mode.to_flags(),
                self.as_c_arg(),
                pid,
                reference,
            )
        };

        SelectReturn(res).into()
    }

    // TODO: select_read/select_write with an optional custom message

    pub fn cancel<'a>(&self, env: Env<'a>, event: &Event, mode: SelectMode) -> SelectResult {
        let res = unsafe {
            enif_select(
                env.as_c_arg(),
                event.0,
                mode.to_flags() | ErlNifSelectFlags::ERL_NIF_SELECT_CANCEL as c_int,
                self.as_c_arg(),
                std::ptr::null(),
                0usize,
            )
        };

        SelectReturn(res).into()
    }

    pub fn stop<'a>(&self, env: Env<'a>, event: &Event) -> SelectResult {
        let res = unsafe {
            enif_select(
                env.as_c_arg(),
                event.0,
                ErlNifSelectFlags::ERL_NIF_SELECT_STOP as c_int,
                self.as_c_arg(),
                std::ptr::null(),
                0usize,
            )
        };

        SelectReturn(res).into()
    }
}
