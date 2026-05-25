use super::{
    nif_filler::{self, DynNifFiller},
    types::*,
};

#[cfg(not(feature = "nif_version_2_14"))]
compile_error!("At least the minimal feature nif_version_2_14 has to be defined");

#[cfg(not(any(unix, windows)))]
compile_error!("Unsupported Operating System Family");

#[cfg(all(
    unix,
    not(any(target_pointer_width = "32", target_pointer_width = "64"))
))]
compile_error!("Unsupported target pointer width");

#[cfg(any(windows, target_pointer_width = "32"))]
use std::os::raw::{c_longlong, c_ulonglong};

pub const ERL_NIF_ENTRY_OPTIONS: c_uint = ERL_NIF_DIRTY_NIF_OPTION;
pub const NIF_MAJOR_VERSION: c_int = 2;

const fn nif_minor_version() -> c_int {
    if cfg!(feature = "nif_version_2_18") {
        18
    } else if cfg!(feature = "nif_version_2_17") {
        17
    } else if cfg!(feature = "nif_version_2_16") {
        16
    } else if cfg!(feature = "nif_version_2_15") {
        15
    } else {
        14
    }
}

pub const NIF_MINOR_VERSION: c_int = nif_minor_version();

static mut DYN_NIF_CALLBACKS: DynNifCallbacks =
    unsafe { std::mem::MaybeUninit::zeroed().assume_init() };

pub unsafe fn internal_set_symbols(callbacks: DynNifCallbacks) {
    DYN_NIF_CALLBACKS = callbacks;
}

#[allow(static_mut_refs)]
pub unsafe fn internal_write_symbols() {
    let filler = nif_filler::new();
    DYN_NIF_CALLBACKS.write_symbols(filler);
}

/// See [enif_make_pid](http://erlang.org/doc/man/erl_nif.html#enif_make_pid) in the Erlang docs
pub unsafe fn enif_make_pid(_env: *mut ErlNifEnv, pid: ErlNifPid) -> ERL_NIF_TERM {
    pid.pid
}

/// See [enif_compare_pids](http://erlang.org/doc/man/erl_nif.html#enif_compare_pids) in the Erlang docs
pub unsafe fn enif_compare_pids(pid1: *const ErlNifPid, pid2: *const ErlNifPid) -> c_int {
    // Mimics the implementation of the enif_compare_pids macro
    enif_compare((*pid1).pid, (*pid2).pid)
}

define_nif_api! {
    fn enif_priv_data(arg1: *mut ErlNifEnv) -> *mut c_void;
    fn enif_alloc(size: size_t) -> *mut c_void;
    fn enif_free(ptr: *mut c_void);
    fn enif_is_atom(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_is_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_is_ref(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_inspect_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int;
    fn enif_alloc_binary(size: size_t, bin: *mut ErlNifBinary) -> c_int;
    fn enif_realloc_binary(bin: *mut ErlNifBinary, size: size_t) -> c_int;
    fn enif_release_binary(bin: *mut ErlNifBinary);
    fn enif_get_int(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_int) -> c_int;
    fn enif_get_ulong(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulong) -> c_int;
    fn enif_get_double(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, dp: *mut c_double) -> c_int;
    fn enif_get_list_cell(env: *mut ErlNifEnv, term: ERL_NIF_TERM, head: *mut ERL_NIF_TERM, tail: *mut ERL_NIF_TERM) -> c_int;
    fn enif_get_tuple(env: *mut ErlNifEnv, tpl: ERL_NIF_TERM, arity: *mut c_int, array: *mut *const ERL_NIF_TERM) -> c_int;
    fn enif_is_identical(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c_int;
    fn enif_compare(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c_int;
    fn enif_make_binary(env: *mut ErlNifEnv, bin: *mut ErlNifBinary) -> ERL_NIF_TERM;
    fn enif_make_badarg(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    fn enif_make_int(env: *mut ErlNifEnv, i: c_int) -> ERL_NIF_TERM;
    fn enif_make_ulong(env: *mut ErlNifEnv, i: c_ulong) -> ERL_NIF_TERM;
    fn enif_make_double(env: *mut ErlNifEnv, d: c_double) -> ERL_NIF_TERM;
    fn enif_make_atom(env: *mut ErlNifEnv, name: *const c_uchar) -> ERL_NIF_TERM;
    fn enif_make_existing_atom(env: *mut ErlNifEnv, name: *const c_uchar, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding) -> c_int;
    variadic fn enif_make_tuple => get_enif_make_tuple(env: *mut ErlNifEnv, cnt: c_uint) -> ERL_NIF_TERM;
    variadic fn enif_make_list => get_enif_make_list(env: *mut ErlNifEnv, cnt: c_uint) -> ERL_NIF_TERM;
    fn enif_make_list_cell(env: *mut ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM) -> ERL_NIF_TERM;
    fn enif_make_string(env: *mut ErlNifEnv, string: *const c_uchar, arg1: ErlNifCharEncoding) -> ERL_NIF_TERM;
    fn enif_make_ref(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    dummy dummy_enif_mutex_create;
    dummy dummy_enif_mutex_destroy;
    dummy dummy_enif_mutex_trylock;
    dummy dummy_enif_mutex_lock;
    dummy dummy_enif_mutex_unlock;
    dummy dummy_enif_cond_create;
    dummy dummy_enif_cond_destroy;
    dummy dummy_enif_cond_signal;
    dummy dummy_enif_cond_broadcast;
    dummy dummy_enif_cond_wait;
    dummy dummy_enif_rwlock_create;
    dummy dummy_enif_rwlock_destroy;
    dummy dummy_enif_rwlock_tryrlock;
    dummy dummy_enif_rwlock_rlock;
    dummy dummy_enif_rwlock_runlock;
    dummy dummy_enif_rwlock_tryrwlock;
    dummy dummy_enif_rwlock_rwlock;
    dummy dummy_enif_rwlock_rwunlock;
    dummy dummy_enif_tsd_key_create;
    dummy dummy_enif_tsd_key_destroy;
    dummy dummy_enif_tsd_set;
    dummy dummy_enif_tsd_get;
    dummy dummy_enif_thread_opts_create;
    dummy dummy_enif_thread_opts_destroy;
    dummy dummy_enif_thread_create;
    dummy dummy_enif_thread_self;
    dummy dummy_enif_equal_tids;
    dummy dummy_enif_thread_exit;
    dummy dummy_enif_thread_join;
    fn enif_realloc(ptr: *mut c_void, size: size_t) -> *mut c_void;
    fn enif_system_info(sip: *mut ErlNifSysInfo, si_size: size_t);
    variadic fn enif_fprintf => get_enif_fprintf(filep: *mut c_void, format: *const c_uchar) -> c_int;
    fn enif_inspect_iolist_as_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int;
    fn enif_make_sub_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, pos: size_t, size: size_t) -> ERL_NIF_TERM;
    fn enif_get_string(arg1: *mut ErlNifEnv, list: ERL_NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding) -> c_int;
    fn enif_get_atom(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding) -> c_int;
    fn enif_is_fun(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_is_pid(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_is_port(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_get_uint(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_uint) -> c_int;
    fn enif_get_long(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_long) -> c_int;
    fn enif_make_uint(arg1: *mut ErlNifEnv, i: c_uint) -> ERL_NIF_TERM;
    fn enif_make_long(arg1: *mut ErlNifEnv, i: c_long) -> ERL_NIF_TERM;
    fn enif_make_tuple_from_array(arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint) -> ERL_NIF_TERM;
    fn enif_make_list_from_array(arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint) -> ERL_NIF_TERM;
    fn enif_is_empty_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_open_resource_type(arg1: *mut ErlNifEnv, module_str: *const c_char, name_str: *const c_char, dtor: Option<unsafe extern "C" fn(arg1: *mut ErlNifEnv, arg2: *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType;
    fn enif_alloc_resource(_type: *const ErlNifResourceType, size: size_t) -> *mut c_void;
    fn enif_release_resource(obj: *const c_void);
    fn enif_make_resource(arg1: *mut ErlNifEnv, obj: *const c_void) -> ERL_NIF_TERM;
    fn enif_get_resource(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, _type: *const ErlNifResourceType, objp: *mut *const c_void) -> c_int;
    fn enif_sizeof_resource(obj: *const c_void) -> size_t;
    fn enif_make_new_binary(arg1: *mut ErlNifEnv, size: size_t, termp: *mut ERL_NIF_TERM) -> *mut c_uchar;
    fn enif_is_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_is_tuple(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_get_atom_length(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, len: *mut c_uint, arg2: ErlNifCharEncoding) -> c_int;
    fn enif_get_list_length(env: *mut ErlNifEnv, term: ERL_NIF_TERM, len: *mut c_uint) -> c_int;
    fn enif_make_atom_len(env: *mut ErlNifEnv, name: *const c_char, len: size_t) -> ERL_NIF_TERM;
    fn enif_make_existing_atom_len(env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding) -> c_int;
    fn enif_make_string_len(env: *mut ErlNifEnv, string: *const c_char, len: size_t, arg1: ErlNifCharEncoding) -> ERL_NIF_TERM;
    fn enif_alloc_env() -> *mut ErlNifEnv;
    fn enif_free_env(env: *mut ErlNifEnv);
    fn enif_clear_env(env: *mut ErlNifEnv);
    fn enif_send(env: *mut ErlNifEnv, to_pid: *const ErlNifPid, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM) -> c_int;
    fn enif_make_copy(dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM) -> ERL_NIF_TERM;
    fn enif_self(caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid) -> *mut ErlNifPid;
    fn enif_get_local_pid(env: *mut ErlNifEnv, arg1: ERL_NIF_TERM, pid: *mut ErlNifPid) -> c_int;
    fn enif_keep_resource(obj: *const c_void);
    fn enif_make_resource_binary(arg1: *mut ErlNifEnv, obj: *const c_void, data: *const c_void, size: size_t) -> ERL_NIF_TERM;
    #[cfg(any(windows, target_pointer_width = "32"))] {
        fn enif_get_int64(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_longlong) -> c_int;
        fn enif_get_uint64(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulonglong) -> c_int;
        fn enif_make_int64(env: *mut ErlNifEnv, i: c_longlong) -> ERL_NIF_TERM;
        fn enif_make_uint64(env: *mut ErlNifEnv, i: c_ulonglong) -> ERL_NIF_TERM;
    }
    fn enif_is_exception(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_make_reverse_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, list: *mut ERL_NIF_TERM) -> c_int;
    fn enif_is_number(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_dlopen(lib: *const c_char, err_handler: Option<unsafe extern "C" fn(arg1: *mut c_void, arg2: *const c_char)>, err_arg: *mut c_void) -> *mut c_void;
    fn enif_dlsym(handle: *mut c_void, symbol: *const c_char, err_handler: Option<unsafe extern "C" fn(arg1: *mut c_void, arg2: *const c_char)>, err_arg: *mut c_void) -> *mut c_void;
    fn enif_consume_timeslice(arg1: *mut ErlNifEnv, percent: c_int) -> c_int;
    fn enif_is_map(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    fn enif_get_map_size(env: *mut ErlNifEnv, term: ERL_NIF_TERM, size: *mut size_t) -> c_int;
    fn enif_make_new_map(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    fn enif_make_map_put(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int;
    fn enif_get_map_value(env: *mut ErlNifEnv, map: ERL_NIF_TERM, key: ERL_NIF_TERM, value: *mut ERL_NIF_TERM) -> c_int;
    fn enif_make_map_update(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int;
    fn enif_make_map_remove(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int;
    fn enif_map_iterator_create(env: *mut ErlNifEnv, map: ERL_NIF_TERM, iter: *mut ErlNifMapIterator, entry: ErlNifMapIteratorEntry) -> c_int;
    fn enif_map_iterator_destroy(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator);
    fn enif_map_iterator_is_head(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    fn enif_map_iterator_is_tail(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    fn enif_map_iterator_next(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    fn enif_map_iterator_prev(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    fn enif_map_iterator_get_pair(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator, key: *mut ERL_NIF_TERM, value: *mut ERL_NIF_TERM) -> c_int;
    fn enif_schedule_nif(env: *mut ErlNifEnv, fun_name: *const c_char, flags: c_int, fp: unsafe extern "C" fn(env: *mut ErlNifEnv, argc: c_int, argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM, argc: c_int, argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM;
    fn enif_has_pending_exception(env: *mut ErlNifEnv, reason: *mut ERL_NIF_TERM) -> c_int;
    fn enif_raise_exception(env: *mut ErlNifEnv, reason: ERL_NIF_TERM) -> ERL_NIF_TERM;
    fn enif_getenv(key: *const c_char, value: *mut c_char, value_size: *mut size_t) -> c_int;
    fn enif_monotonic_time(unit: ErlNifTimeUnit) -> ErlNifTime;
    fn enif_time_offset(unit: ErlNifTimeUnit) -> ErlNifTime;
    fn enif_convert_time_unit(time: ErlNifTime, from_unit: ErlNifTimeUnit, to_unit: ErlNifTimeUnit) -> ErlNifTime;
    fn enif_now_time(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    fn enif_cpu_time(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    fn enif_make_unique_integer(env: *mut ErlNifEnv, properties: ErlNifUniqueInteger) -> ERL_NIF_TERM;
    fn enif_is_current_process_alive(env: *mut ErlNifEnv) -> c_int;
    fn enif_is_process_alive(env: *mut ErlNifEnv, pid: *const ErlNifPid) -> c_int;
    fn enif_is_port_alive(env: *mut ErlNifEnv, port_id: *const ErlNifPort) -> c_int;
    fn enif_get_local_port(env: *mut ErlNifEnv, term: ERL_NIF_TERM, port_id: *mut ErlNifPort) -> c_int;
    fn enif_term_to_binary(env: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int;
    fn enif_binary_to_term(env: *mut ErlNifEnv, data: *const c_uchar, sz: usize, term: *mut ERL_NIF_TERM, opts: ErlNifBinaryToTerm) -> usize;
    fn enif_port_command(env: *mut ErlNifEnv, to_port: *const ErlNifPort, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM) -> c_int;
    fn enif_thread_type() -> c_int;
    variadic fn enif_snprintf => get_enif_snprintf(out: *mut c_char, size: usize, format: *const c_char) -> c_int;
    fn enif_select(env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *const c_void, pid: *const ErlNifPid, eref: ERL_NIF_TERM) -> c_int;
    fn enif_open_resource_type_x(env: *mut ErlNifEnv, name_str: *const c_char, init: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType;
    fn enif_monitor_process(env: *mut ErlNifEnv, obj: *const c_void, pid: *const ErlNifPid, monitor: *mut ErlNifMonitor) -> c_int;
    fn enif_demonitor_process(env: *mut ErlNifEnv, obj: *const c_void, monitor: *const ErlNifMonitor) -> c_int;
    fn enif_compare_monitors(monitor1: *const ErlNifMonitor, monitor2: *const ErlNifMonitor) -> c_int;
    fn enif_hash(hashtype: ErlNifHash, term: ERL_NIF_TERM, salt: u64) -> u64;
    fn enif_whereis_pid(env: *mut ErlNifEnv, name: ERL_NIF_TERM, pid: *mut ErlNifPid) -> c_int;
    fn enif_whereis_port(env: *mut ErlNifEnv, name: ERL_NIF_TERM, port: *mut ErlNifPort) -> c_int;
    dummy dummy_enif_ioq_create;
    dummy dummy_enif_ioq_destroy;
    dummy dummy_enif_ioq_enq_binary;
    dummy dummy_enif_ioq_enqv;
    dummy dummy_enif_ioq_size;
    dummy dummy_enif_ioq_deq;
    dummy dummy_enif_ioq_peek;
    dummy dummy_enif_inspect_iovec;
    dummy dummy_enif_free_iovec;
    dummy dummy_enif_ioq_peek_head;
    dummy dummy_enif_mutex_name;
    dummy dummy_enif_cond_name;
    dummy dummy_enif_rwlock_name;
    dummy dummy_enif_thread_name;
    dummy dummy_enif_vfprintf;
    dummy dummy_enif_vsnprintf;
    fn enif_make_map_from_arrays(env: *mut ErlNifEnv, keys: *const ERL_NIF_TERM, values: *const ERL_NIF_TERM, cnt: usize, map_out: *mut ERL_NIF_TERM) -> c_int;
    #[cfg(feature = "nif_version_2_15")] {
        dummy enif_select_x;
        fn enif_make_monitor_term(env: *mut ErlNifEnv, mon: *const ErlNifMonitor) -> ERL_NIF_TERM;
        fn enif_is_pid_undefined(pid: *const ErlNifPid) -> c_int;
        fn enif_set_pid_undefined(pid: *mut ErlNifPid);
        fn enif_term_type(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> ErlNifTermType;
    }
    #[cfg(feature = "nif_version_2_16")] {
        fn enif_init_resource_type(env: *mut ErlNifEnv, name_str: *const c_char, init: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType;
        fn enif_dynamic_resource_call(env: *mut ErlNifEnv, module: ERL_NIF_TERM, name: ERL_NIF_TERM, rsrc: ERL_NIF_TERM, call_data: *const c_void) -> c_int;
    }
    #[cfg(feature = "nif_version_2_17")] {
        fn enif_get_string_length(env: *mut ErlNifEnv, list: ERL_NIF_TERM, len: *mut c_uint, encoding: ErlNifCharEncoding) -> c_int;
        fn enif_make_new_atom(env: *mut ErlNifEnv, name: *const c_char, atom: *mut ERL_NIF_TERM, encoding: ErlNifCharEncoding) -> c_int;
        fn enif_make_new_atom_len(env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, encoding: ErlNifCharEncoding) -> c_int;
        fn enif_set_option(env: *mut ErlNifEnv, opt: ErlNifOption) -> c_int;
    }
    #[cfg(feature = "nif_version_2_18")] {
        fn enif_term_size(term: ERL_NIF_TERM) -> usize;
        fn enif_get_atom_cache_index(env: *mut ErlNifEnv, term: ERL_NIF_TERM, index: *mut c_uint) -> c_int;
        fn enif_max_atom_cache_index() -> c_uint;
    }
}

#[macro_export]
macro_rules! enif_make_tuple {
    ($( $arg:expr ),* ) => {{ $crate::sys::get_enif_make_tuple()($($arg),*) }};
    ($( $arg:expr ),+, ) => {{ enif_make_tuple!($($arg),*) }};
}

#[macro_export]
macro_rules! enif_make_list {
    ($( $arg:expr ),* ) => {{ $crate::sys::get_enif_make_list()($($arg),*) }};
    ($( $arg:expr ),+, ) => {{ enif_make_list!($($arg),*) }};
}

#[macro_export]
macro_rules! enif_fprintf {
    ($( $arg:expr ),* ) => {{ $crate::sys::get_enif_fprintf()($($arg),*) }};
    ($( $arg:expr ),+, ) => {{ enif_fprintf!($($arg),*) }};
}

#[macro_export]
macro_rules! enif_snprintf {
    ($( $arg:expr ),* ) => {{ $crate::sys::get_enif_snprintf()($($arg),*) }};
    ($( $arg:expr ),+, ) => {{ enif_snprintf!($($arg),*) }};
}

#[cfg(not(any(windows, target_pointer_width = "32")))]
/// See [enif_make_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int64) at erlang.org
#[inline]
pub unsafe fn enif_make_int64(env: *mut ErlNifEnv, i: i64) -> ERL_NIF_TERM {
    enif_make_long(env, i)
}

#[cfg(not(any(windows, target_pointer_width = "32")))]
/// See [enif_make_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint64) at erlang.org
#[inline]
pub unsafe fn enif_make_uint64(env: *mut ErlNifEnv, i: u64) -> ERL_NIF_TERM {
    enif_make_ulong(env, i)
}

#[cfg(not(any(windows, target_pointer_width = "32")))]
/// See [enif_get_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int64) at erlang.org
#[inline]
pub unsafe fn enif_get_int64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int {
    enif_get_long(env, term, ip)
}

#[cfg(not(any(windows, target_pointer_width = "32")))]
/// See [enif_get_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint64) at erlang.org
#[inline]
pub unsafe fn enif_get_uint64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int {
    enif_get_ulong(env, term, ip)
}
