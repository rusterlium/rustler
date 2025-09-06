#![allow(clippy::missing_safety_doc)]
#![allow(clippy::upper_case_acronyms)]

pub use std::ffi::{c_char, c_double, c_int, c_long, c_uchar, c_uint, c_ulong, c_void};

use std::os;

#[allow(non_camel_case_types)]
pub type size_t = usize;

//use std::mem::size_of;

#[allow(non_camel_case_types)]
pub type ERL_NIF_UINT = size_t;

#[allow(non_camel_case_types)]
pub type ERL_NIF_TERM = ERL_NIF_UINT;

//#[derive(Debug, Copy, Clone)]
//#[repr(C)]
//pub struct ERL_NIF_TERM(ERL_NIF_UINT);  // Don't do this, 32 bit calling convention is different for structs and ints.

/// See [ErlNifEnv](http://www.erlang.org/doc/man/erl_nif.html#ErlNifEnv) in the Erlang docs.
#[derive(Debug)]
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifEnv {
    dummy: *mut c_void, // block automatic Send and Sync traits.  Ref https://doc.rust-lang.org/beta/nomicon/send-and-sync.html
}

// Ownership of an env may be safely transfers between threads, therefore ErlNifEnv is Send.
// This is the common use case for process independent environments created with enif_alloc_env().
// ErlNifEnv is NOT Sync because it is thread unsafe.
unsafe impl Send for ErlNifEnv {}

/// See [ErlNifFunc](http://www.erlang.org/doc/man/erl_nif.html#ErlNifFunc) in the Erlang docs.
// #[allow(missing_copy_implementations)]
#[derive(Debug)]
#[repr(C)]
pub struct ErlNifFunc {
    pub name: *const c_char,
    pub arity: c_uint,
    pub function: unsafe extern "C" fn(
        env: *mut ErlNifEnv,
        argc: c_int,
        argv: *const ERL_NIF_TERM,
    ) -> ERL_NIF_TERM,
    pub flags: c_uint,
}

// #[allow(missing_copy_implementations)]
#[doc(hidden)]
#[derive(Debug)]
#[repr(C)]
#[allow(non_snake_case)]
pub struct ErlNifEntry {
    pub major: c_int,
    pub minor: c_int,
    pub name: *const c_char,
    pub num_of_funcs: c_int,
    pub funcs: *const ErlNifFunc,
    pub load: Option<
        unsafe extern "C" fn(
            env: *mut ErlNifEnv,
            priv_data: *mut *mut c_void,
            load_info: ERL_NIF_TERM,
        ) -> c_int,
    >,
    pub reload: Option<
        unsafe extern "C" fn(
            env: *mut ErlNifEnv,
            priv_data: *mut *mut c_void,
            load_info: ERL_NIF_TERM,
        ) -> c_int,
    >,
    pub upgrade: Option<
        unsafe extern "C" fn(
            env: *mut ErlNifEnv,
            priv_data: *mut *mut c_void,
            old_priv_data: *mut *mut c_void,
            load_info: ERL_NIF_TERM,
        ) -> c_int,
    >,
    pub unload: Option<unsafe extern "C" fn(env: *mut ErlNifEnv, priv_data: *mut c_void) -> ()>,
    pub vm_variant: *const c_char,
    pub options: c_uint,                      // added in 2.7
    pub sizeof_ErlNifResourceTypeInit: usize, // added in 2.12
    pub min_erts: *const c_char,              // added in 2.14
}

pub const ERL_NIF_DIRTY_NIF_OPTION: c_uint = 1;

/// See [ErlNifBinary](http://www.erlang.org/doc/man/erl_nif.html#ErlNifBinary) in the Erlang docs.
#[allow(missing_copy_implementations)]
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct ErlNifBinary {
    pub size: size_t,
    pub data: *mut c_uchar,
    ref_bin: *mut c_void,
    _spare: [*mut c_void; 2],
}

#[cfg(windows)]
pub type ErlNifEvent = os::windows::raw::HANDLE;

#[cfg(unix)]
pub type ErlNifEvent = os::unix::io::RawFd;

/// See [ErlNifResourceType](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceType) in the Erlang docs.
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifResourceType {
    dummy: c_int,
}

/// See [ErlNifResourceDtor](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceDtor) in the Erlang docs.
#[allow(missing_copy_implementations)]
pub type ErlNifResourceDtor = unsafe extern "C" fn(env: *mut ErlNifEnv, obj: *mut c_void) -> ();

/// See [ErlNifResourceStop](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceStop) in the Erlang docs.
#[allow(missing_copy_implementations)]
pub type ErlNifResourceStop = unsafe extern "C" fn(
    env: *mut ErlNifEnv,
    obj: *mut c_void,
    event: ErlNifEvent,
    is_direct_call: c_int,
) -> ();

/// See [ErlNifResourceDown](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceDown) in the Erlang docs.
#[allow(missing_copy_implementations)]
pub type ErlNifResourceDown = unsafe extern "C" fn(
    env: *mut ErlNifEnv,
    obj: *mut c_void,
    pid: *const ErlNifPid,
    mon: *const ErlNifMonitor,
) -> ();

/// See [ErlNifResourceDynCall](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceDynCall) in the Erlang Docs.
pub type ErlNifResourceDynCall =
    unsafe extern "C" fn(env: *mut ErlNifEnv, obj: *mut c_void, call_data: *const c_void) -> ();

/// See [ErlNifResourceTypeInit](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceTypeInit) in the Erlang docs.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct ErlNifResourceTypeInit {
    pub dtor: *const ErlNifResourceDtor,
    pub stop: *const ErlNifResourceStop, // at ERL_NIF_SELECT_STOP event
    pub down: *const ErlNifResourceDown, // enif_monitor_process
    pub members: c_int,
    pub dyncall: *const ErlNifResourceDynCall,
}

/// See [ErlNifSelectFlags](http://erlang.org/doc/man/erl_nif.html#ErlNifSelectFlags) in the Erlang docs.
pub type ErlNifSelectFlags = c_int;
#[allow(clippy::identity_op)]
pub const ERL_NIF_SELECT_READ: ErlNifSelectFlags = 1 << 0;
pub const ERL_NIF_SELECT_WRITE: ErlNifSelectFlags = 1 << 1;
pub const ERL_NIF_SELECT_STOP: ErlNifSelectFlags = 1 << 2;
pub const ERL_NIF_SELECT_FAILED: ErlNifSelectFlags = 1 << 3;
pub const ERL_NIF_SELECT_READ_CANCELLED: ErlNifSelectFlags = 1 << 4;
pub const ERL_NIF_SELECT_WRITE_CANCELLED: ErlNifSelectFlags = 1 << 5;
pub const ERL_NIF_SELECT_ERROR_CANCELLED: ErlNifSelectFlags = 1 << 6;
pub const ERL_NIF_SELECT_NOTSUP: ErlNifSelectFlags = 1 << 7;

/// See [ErlNifMonitor](http://www.erlang.org/doc/man/erl_nif.html#ErlNifMonitor) in the Erlang docs.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct ErlNifMonitor {
    // from https://github.com/erlang/otp/blob/83e20c62057ebc1d8064bf57b01be560cd244e1d/erts/emulator/beam/erl_drv_nif.h#L64
    // data: [c_uchar; size_of::<*const c_void>()*4],  size_of is non-const
    data: [usize; 4],
}

/// See [ErlNifResourceFlags](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceFlags) in the Erlang docs.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub enum ErlNifResourceFlags {
    ERL_NIF_RT_CREATE = 1,
    ERL_NIF_RT_TAKEOVER = 2,
}

/// See [ErlNifCharEncoding](http://www.erlang.org/doc/man/erl_nif.html#ErlNifCharEncoding) in the Erlang docs.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub enum ErlNifCharEncoding {
    ERL_NIF_LATIN1 = 1,
    #[cfg(feature = "nif_version_2_17")]
    ERL_NIF_UTF8 = 2,
}

/// See [ErlNifPid](http://www.erlang.org/doc/man/erl_nif.html#ErlNifPid) in the Erlang docs.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct ErlNifPid {
    pub(crate) pid: ERL_NIF_TERM,
}

/// See [ErlNifSysInfo](http://www.erlang.org/doc/man/erl_nif.html#ErlNifSysInfo) in the Erlang docs.
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifSysInfo {
    pub driver_major_version: c_int,
    pub driver_minor_version: c_int,
    pub erts_version: *mut c_char,
    pub otp_release: *mut c_char,
    pub thread_support: c_int,
    pub smp_support: c_int,
    pub async_threads: c_int,
    pub scheduler_threads: c_int,
    pub nif_major_version: c_int,
    pub nif_minor_version: c_int,
    pub dirty_scheduler_support: c_int,
}

// /// See [ErlNifFunc](http://www.erlang.org/doc/man/erl_nif.html#ErlNifFunc) in the Erlang docs.
// #[derive(Copy, Clone)]
// #[repr(C)]
// pub enum ErlNifDirtyTaskFlags {
//     ERL_NIF_DIRTY_JOB_CPU_BOUND = 1,
//     ERL_NIF_DIRTY_JOB_IO_BOUND = 2,
// }

pub type ErlNifDirtyTaskFlags = c_uint;
pub const ERL_NIF_DIRTY_JOB_CPU_BOUND: ErlNifDirtyTaskFlags = 1;
pub const ERL_NIF_DIRTY_JOB_IO_BOUND: ErlNifDirtyTaskFlags = 2;

/// See [ErlNifMapIterator](http://www.erlang.org/doc/man/erl_nif.html#ErlNifMapIterator) in the Erlang docs.
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifMapIterator {
    map: ERL_NIF_TERM,
    t_limit: ERL_NIF_UINT,
    idx: ERL_NIF_UINT,
    ks: *mut ERL_NIF_TERM,
    vs: *mut ERL_NIF_TERM,
    __spare__: [*mut c_void; 2],
}

/// See [ErlNifMapIteratorEntry](http://www.erlang.org/doc/man/erl_nif.html#ErlNifMapIteratorEntry) in the Erlang docs.
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifMapIteratorEntry {
    ERL_NIF_MAP_ITERATOR_HEAD = 1,
    ERL_NIF_MAP_ITERATOR_TAIL = 2,
}

/// See [ErlNifTime](http://www.erlang.org/doc/man/erl_nif.html#ErlNifTime) in the Erlang docs.
pub type ErlNifTime = i64;

/// Error return value for `enif_monotonic_time()`, `enif_time_offset()`, and `enif_convert_time_unit()`.
pub const ERL_NIF_TIME_ERROR: i64 = -9_223_372_036_854_775_808;
//const ERL_NIF_TIME_ERROR:i64 = i64::min_value();  "error: const fn's not yet stable"

/// See [ErlNifTimeUnit](http://www.erlang.org/doc/man/erl_nif.html#ErlNifTimeUnit) in the Erlang docs.
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifTimeUnit {
    // values yanked from https://github.com/erlang/otp/blob/7cb403e4aa044fd2cc7702dbe8e2d0eea68e81f3/erts/emulator/beam/erl_drv_nif.h#L132
    ERL_NIF_SEC = 0,
    ERL_NIF_MSEC = 1,
    ERL_NIF_USEC = 2,
    ERL_NIF_NSEC = 3,
}

/// See [ErlNifUniqueInteger](http://erlang.org/doc/man/erl_nif.html#ErlNifUniqueInteger) in the Erlang docs.
pub type ErlNifUniqueInteger = c_int;
#[allow(clippy::identity_op)]
pub const ERL_NIF_UNIQUE_POSITIVE: ErlNifUniqueInteger = 1 << 0;
#[allow(clippy::identity_op)]
pub const ERL_NIF_UNIQUE_MONOTONIC: ErlNifUniqueInteger = 1 << 1;
// ref https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_nif.h#L203
// FIXME: Should actually be C enum, but repr(C) enums in Rust can't be used as bitfields.
//        Fix if the right abstraction ever lands in Rust.

/// See [ErlNifPort](http://erlang.org/doc/man/erl_nif.html#ErlNifPort) in the Erlang docs.
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ErlNifPort {
    port_id: ERL_NIF_TERM, // internal, may change
}
// ref https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_nif.h#L155

/// See [ErlNifBinaryToTerm](http://erlang.org/doc/man/erl_nif.html#ErlNifBinaryToTerm) in the Erlang docs.
pub type ErlNifBinaryToTerm = c_int;
pub const ERL_NIF_BIN2TERM_SAFE: ErlNifBinaryToTerm = 0x2000_0000;

pub const ERL_NIF_THR_UNDEFINED: c_int = 0;
pub const ERL_NIF_THR_NORMAL_SCHEDULER: c_int = 1;
pub const ERL_NIF_THR_DIRTY_CPU_SCHEDULER: c_int = 2;
pub const ERL_NIF_THR_DIRTY_IO_SCHEDULER: c_int = 3;

/// See [ErlNifHash](http://www.erlang.org/doc/man/erl_nif.html#ErlNifHash) in the Erlang docs.
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifHash {
    // from https://github.com/erlang/otp/blob/83e20c62057ebc1d8064bf57b01be560cd244e1d/erts/emulator/beam/erl_nif.h#L242
    ERL_NIF_INTERNAL_HASH = 1,
    ERL_NIF_PHASH2 = 2,
}

/// See [ErlNifTermType](http://www.erlang.org/doc/man/erl_nif.html#ErlNifTermType) in the Erlang docs.
#[cfg(feature = "nif_version_2_15")]
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifTermType {
    // from https://github.com/erlang/otp/blob/6618ce7b6a621e92db72ea4f01f7d38553c8818c/erts/emulator/beam/erl_nif.h#L291
    ERL_NIF_TERM_TYPE_ATOM = 1,
    ERL_NIF_TERM_TYPE_BITSTRING = 2,
    ERL_NIF_TERM_TYPE_FLOAT = 3,
    ERL_NIF_TERM_TYPE_FUN = 4,
    ERL_NIF_TERM_TYPE_INTEGER = 5,
    ERL_NIF_TERM_TYPE_LIST = 6,
    ERL_NIF_TERM_TYPE_MAP = 7,
    ERL_NIF_TERM_TYPE_PID = 8,
    ERL_NIF_TERM_TYPE_PORT = 9,
    ERL_NIF_TERM_TYPE_REFERENCE = 10,
    ERL_NIF_TERM_TYPE_TUPLE = 11,

    /* This is a dummy value intended to coax the compiler into warning about
     * unhandled values in a switch even if all the above values have been
     * handled. We can add new entries at any time so the user must always
     * have a default case. */
    ERL_NIF_TERM_TYPE__MISSING_DEFAULT_CASE__READ_THE_MANUAL = -1,
}

/// See [ErlNifOption](http://www.erlang.org/doc/man/erl_nif.html#ErlNifOption) in the Erlang docs.
#[cfg(feature = "nif_version_2_17")]
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifOption {
    // from https://github.com/erlang/otp/blob/1b3c6214d4bf359f7e5c143bef5c5ad9c90c5536/erts/emulator/beam/erl_nif.h#L333
    ERL_NIF_OPT_DELAY_HALT = 1,
    ERL_NIF_OPT_ON_HALT = 2,
}
