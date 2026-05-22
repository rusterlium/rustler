use std::ffi::c_int;

use crate::sys::{
    enif_select, ErlNifEvent, ErlNifSelectFlags, ERL_NIF_SELECT_ERROR_CANCELLED,
    ERL_NIF_SELECT_FAILED, ERL_NIF_SELECT_INVALID_EVENT, ERL_NIF_SELECT_NOTSUP,
    ERL_NIF_SELECT_READ_CANCELLED, ERL_NIF_SELECT_STOP_CALLED, ERL_NIF_SELECT_STOP_SCHEDULED,
    ERL_NIF_SELECT_WRITE_CANCELLED,
};
use crate::types::atom::undefined;
use crate::{Encoder, Env, LocalPid, Reference, Resource, ResourceArc, Term};

#[derive(Clone, Copy, Debug)]
pub struct SelectResult(c_int);

macro_rules! getter {
    ($name:ident, $flag:ident) => {
        pub fn $name(self) -> bool {
            self.0 & $flag != 0
        }
    };
}

impl SelectResult {
    getter! {stop_called, ERL_NIF_SELECT_STOP_CALLED}
    getter! {stop_scheduled, ERL_NIF_SELECT_STOP_SCHEDULED}
    getter! {invalid_event, ERL_NIF_SELECT_INVALID_EVENT}
    getter! {failed, ERL_NIF_SELECT_FAILED}
    getter! {read_cancelled, ERL_NIF_SELECT_READ_CANCELLED}
    getter! {write_cancelled, ERL_NIF_SELECT_WRITE_CANCELLED}
    getter! {error_cancelled, ERL_NIF_SELECT_ERROR_CANCELLED}
    getter! {not_supported, ERL_NIF_SELECT_NOTSUP}
}

#[derive(Clone, Copy, Debug)]
pub enum SelectError {}

#[derive(Clone, Copy, Debug)]
pub struct Event(ErlNifEvent);

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

impl Event {
    pub fn from_system(obj: ErlNifEvent) -> Self {
        Self(obj)
    }

    pub fn to_system(self) -> ErlNifEvent {
        self.0
    }
}

impl<T> ResourceArc<T>
where
    T: Resource,
{
    pub fn select<'a>(
        &mut self,
        env: Env<'a>,
        event: &Event,
        mode: SelectMode,
        pid: LocalPid,
        reference: Option<Reference>,
    ) {
        let reference = match reference {
            Some(reference) => reference.encode(env),
            None => undefined().encode(env),
        }
        .as_c_arg();

        let res = unsafe {
            enif_select(
                env.as_c_arg(),
                event.0,
                mode.to_flags(),
                self.as_c_arg(),
                pid.as_c_arg(),
                reference,
            )
        };

        unimplemented!()
    }

    pub fn cancel<'a>(&mut self, env: Env<'a>, event: &Event, mode: SelectMode) {
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

        unimplemented!()
    }

    pub fn stop<'a>(&mut self, env: Env<'a>, event: &Event) {
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
        unimplemented!()
    }
}
