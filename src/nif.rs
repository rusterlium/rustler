
pub use libc::c_int;
use libc::c_uint;
use libc::c_char;
use libc::c_uchar;
use libc::c_void;
use libc::size_t;
use libc::c_ulong;
use libc::c_long;
use libc::c_double;
use std::option::Option;
//use core::marker::Sync;

include!(concat!(env!("OUT_DIR"), "/nif_versions.snippet"));
// example of included content:
// const NIF_MAJOR_VERSION: c_int = 2;
// const NIF_MINOR_VERSION: c_int = 7;


// FIXME
#[allow(non_camel_case_types)]
pub type ERL_NIF_UINT = size_t;
//type ERL_NIF_UINT = usize;  // users complain about non-ffi type.


#[allow(non_camel_case_types)]
pub type ERL_NIF_TERM = *const c_void;
//pub type ERL_NIF_TERM = ERL_NIF_UINT;

/// See [ErlNifEnv](http://www.erlang.org/doc/man/erl_nif.html#ErlNifEnv)
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifEnv;

/// See [ErlNifFunc](http://www.erlang.org/doc/man/erl_nif.html#ErlNifFunc)
// #[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifFunc {
    pub name:     *const u8,
    pub arity:    c_uint,
    pub function: extern "C" fn(env: *mut ErlNifEnv, argc: c_int, argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM,
    pub flags:    c_uint,
}
// unsafe impl Sync for ErlNifFunc {}

// #[allow(missing_copy_implementations)]
#[doc(hidden)]
#[repr(C)]
pub struct ErlNifEntry {
    pub major:        c_int,
    pub minor:        c_int,
    pub name:         *const u8,
    pub num_of_funcs: c_int,
    pub funcs:        *const ErlNifFunc,
    pub load:    Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int>,
    pub reload:  Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
    pub upgrade: Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, old_priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
    pub unload:  Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut c_void) -> ()>,
    pub vm_variant: *const u8,
    pub options: c_uint,
}
//unsafe impl Sync for ErlNifEntry {}

/// See [ErlNifBinary](http://www.erlang.org/doc/man/erl_nif.html#ErlNifBinary)
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifBinary {
    pub size: size_t,
    pub data: *const u8,
    bin_term: ERL_NIF_TERM,
    ref_bin: *mut c_void,
}

/// See [ErlNifResourceType](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceType)
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifResourceType;

/// See [ErlNifResourceDtor](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceDtor)
#[allow(missing_copy_implementations)]
pub type ErlNifResourceDtor = extern "C" fn(arg1: *mut ErlNifEnv, arg2: *mut c_void) -> ();

/// See [ErlNifResourceFlags](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceFlags)
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifResourceFlags {
    ERL_NIF_RT_CREATE = 1,
    ERL_NIF_RT_TAKEOVER = 2,
}

/// See [ErlNifCharEncoding](http://www.erlang.org/doc/man/erl_nif.html#ErlNifCharEncoding)
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifCharEncoding {
    ERL_NIF_LATIN1 = 1,
    DUMMY = 999, // prevents "univariant enum" compile error
}

/// See [ErlNifPid](http://www.erlang.org/doc/man/erl_nif.html#ErlNifPid)
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ErlNifPid {
    pid: ERL_NIF_TERM,
}

/// See [ErlNifSysInfo](http://www.erlang.org/doc/man/erl_nif.html#ErlNifSysInfo)
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

/// See [ErlNifFunc](http://www.erlang.org/doc/man/erl_nif.html#ErlNifFunc)
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifDirtyTaskFlags {
    ERL_NIF_DIRTY_JOB_CPU_BOUND = 1,
    ERL_NIF_DIRTY_JOB_IO_BOUND = 2,
}

/// Experimental in v17
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifMapIterator {
    map: ERL_NIF_TERM,
    t_limit: ERL_NIF_UINT,
    idx: ERL_NIF_UINT,
    ks: *mut ERL_NIF_TERM,
    vs: *mut ERL_NIF_TERM,
    __spare__: [*mut c_void; 2us],
}

/// Experimental in v17
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifMapIteratorEntry {
    ERL_NIF_MAP_ITERATOR_HEAD = 1,
    ERL_NIF_MAP_ITERATOR_TAIL = 2,
}




include!(concat!(env!("OUT_DIR"), "/nif_api.snippet"));
// example of included content:
// extern "C" {
//     pub fn enif_priv_data(arg1: *mut ErlNifEnv) -> *mut c_void;
//     pub fn enif_alloc(size: size_t) -> *mut c_void;
//     pub fn enif_free(ptr: *mut c_void);
//     pub fn enif_is_atom(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
//     pub fn enif_is_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
// ...

/// See [enif_make_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int64) at erlang.org
#[cfg(target_pointer_width = "64")]
pub fn enif_make_int64(env: *mut ErlNifEnv, i: i64) -> ERL_NIF_TERM
    { unsafe {enif_make_long(env, i)}}

/// See [enif_make_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint64) at erlang.org
#[cfg(target_pointer_width = "64")]
pub fn enif_make_uint64(env: *mut ErlNifEnv, i: u64) -> ERL_NIF_TERM
    { unsafe {enif_make_ulong(env, i) }}

/// See [enif_get_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int64) at erlang.org
#[cfg(target_pointer_width = "64")]
pub fn enif_get_int64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int
    { unsafe {enif_get_long(env, term, ip) }}

/// See [enif_get_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint64) at erlang.org
#[cfg(target_pointer_width = "64")]
pub fn enif_get_uint64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int
    { unsafe {enif_get_ulong(env, term, ip) }}

#[doc(hidden)]
#[macro_export]
macro_rules! nif{
    ($name:expr, $arity:expr, $function:expr, $flags:expr) => (
        ErlNifFunc { name:     $name as *const u8,
                     arity:    $arity,
                     function: $function,
                     flags:    $flags});

    ($name:expr, $arity:expr, $function:expr) => (
        nif!($name, $arity, $function, 0))
}

#[doc(hidden)]
#[macro_export]
macro_rules! count_expr {
    () => { 0 };
    ($_e:expr) => { 1 };
    ($_e:expr, $($rest:expr),+) => { 1 + count_expr!($($rest),*) }
}


/// Register NIF and supporting functions for your module.
///
/// C strings must be specified as byte arrays with a terminating null as shown below.
///
/// See [load](http://www.erlang.org/doc/man/erl_nif.html#load), [reload](http://www.erlang.org/doc/man/erl_nif.html#reload),
/// [upgrade](http://www.erlang.org/doc/man/erl_nif.html#upgrade), and [unload](http://www.erlang.org/doc/man/erl_nif.html#unload)
/// in the Erlang docs.
///
/// # Examples
///
/// ```
/// #[macro_use]
/// extern crate ruster_unsafe;
/// use ruster_unsafe::nif::*;
/// 
/// nif_init!(b"my_nif_module\0", Some(load), Some(reload), Some(upgrade), Some(unload),
///     nif!(b"my_nif_fun1", 1, my_nif_fun1),
///     nif!(b"my_dirty_fun2", 1, my_dirty_fun2, ERL_NIF_DIRTY_JOB_CPU_BOUND)
/// );
/// 
/// extern "C" fn load(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int { 0 }
/// extern "C" fn reload(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int { 0 }
/// extern "C" fn upgrade(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, old_priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int { 0 }
/// extern "C" fn unload(arg1: *mut ErlNifEnv, priv_data: *mut c_void) {}
/// # fn main(){}
/// ```
#[macro_export]
macro_rules! nif_init {
    ($module:expr, $load:expr, $reload:expr, $upgrade:expr, $unload:expr, $($func:expr),* ) => (
        const NUM_FUNCS: usize = count_expr!($($func),*);
        const FUNCS: [ErlNifFunc; NUM_FUNCS] = [$($func),*];
        static mut ENTRY: ErlNifEntry = ErlNifEntry{
            major : NIF_MAJOR_VERSION,
            minor : NIF_MINOR_VERSION,
            name : $module as *const u8,
            num_of_funcs : NUM_FUNCS as c_int,
            funcs : &FUNCS as *const ErlNifFunc,
            load :    $load,
            reload :  $reload,
            upgrade : $upgrade,
            unload :  $unload,
            vm_variant : b"beam.vanilla\0" as *const u8,
            options: 0,
        };

        #[no_mangle]
        pub extern "C" fn nif_init() -> *const ErlNifEntry {
            unsafe {&ENTRY}
        }
    )
}

