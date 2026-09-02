pub const ERL_NIF_ENTRY_OPTIONS: c_uint = ERL_NIF_DIRTY_NIF_OPTION;
#[allow(dead_code)]
#[derive(Default, Copy, Clone)]
pub struct DynNifCallbacks {}
impl DynNifCallbacks { fn write_symbols(&mut self, _: impl DynNifFiller) {} }
extern "C" {
    pub fn enif_priv_data(arg1: *mut ErlNifEnv) -> *mut c_void;
    pub fn enif_alloc(size: size_t) -> *mut c_void;
    pub fn enif_free(ptr: *mut c_void);
    pub fn enif_is_atom(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_is_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_is_ref(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_inspect_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int;
    pub fn enif_alloc_binary(size: size_t, bin: *mut ErlNifBinary) -> c_int;
    pub fn enif_realloc_binary(bin: *mut ErlNifBinary, size: size_t) -> c_int;
    pub fn enif_release_binary(bin: *mut ErlNifBinary);
    pub fn enif_get_int(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_int) -> c_int;
    pub fn enif_get_ulong(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulong) -> c_int;
    pub fn enif_get_double(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, dp: *mut c_double) -> c_int;
    pub fn enif_get_list_cell(env: *mut ErlNifEnv, term: ERL_NIF_TERM, head: *mut ERL_NIF_TERM, tail: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_get_tuple(env: *mut ErlNifEnv, tpl: ERL_NIF_TERM, arity: *mut c_int, array: *mut *const ERL_NIF_TERM) -> c_int;
    pub fn enif_is_identical(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c_int;
    pub fn enif_compare(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c_int;
    pub fn enif_make_binary(env: *mut ErlNifEnv, bin: *mut ErlNifBinary) -> ERL_NIF_TERM;
    pub fn enif_make_badarg(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    pub fn enif_make_int(env: *mut ErlNifEnv, i: c_int) -> ERL_NIF_TERM;
    pub fn enif_make_ulong(env: *mut ErlNifEnv, i: c_ulong) -> ERL_NIF_TERM;
    pub fn enif_make_double(env: *mut ErlNifEnv, d: c_double) -> ERL_NIF_TERM;
    pub fn enif_make_atom(env: *mut ErlNifEnv, name: *const c_char) -> ERL_NIF_TERM;
    pub fn enif_make_existing_atom(env: *mut ErlNifEnv, name: *const c_char, atom: *mut ERL_NIF_TERM, arg4: ErlNifCharEncoding) -> c_int;
    #[link_name = "enif_make_tuple"]
    fn __variadic_enif_make_tuple(env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM;
    #[link_name = "enif_make_list"]
    fn __variadic_enif_make_list(env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM;
    pub fn enif_make_list_cell(env: *mut ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM) -> ERL_NIF_TERM;
    pub fn enif_make_string(env: *mut ErlNifEnv, string: *const c_char, arg3: ErlNifCharEncoding) -> ERL_NIF_TERM;
    pub fn enif_make_ref(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    pub fn enif_realloc(ptr: *mut c_void, size: size_t) -> *mut c_void;
    pub fn enif_system_info(sip: *mut ErlNifSysInfo, si_size: size_t);
    #[link_name = "enif_fprintf"]
    fn __variadic_enif_fprintf(filep: *mut c_void, format: *const c_char, ...) -> c_int;
    pub fn enif_inspect_iolist_as_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int;
    pub fn enif_make_sub_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, pos: size_t, size: size_t) -> ERL_NIF_TERM;
    pub fn enif_get_string(arg1: *mut ErlNifEnv, list: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg5: ErlNifCharEncoding) -> c_int;
    pub fn enif_get_atom(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg5: ErlNifCharEncoding) -> c_int;
    pub fn enif_is_fun(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_is_pid(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_is_port(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_get_uint(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_uint) -> c_int;
    pub fn enif_get_long(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_long) -> c_int;
    pub fn enif_make_uint(arg1: *mut ErlNifEnv, i: c_uint) -> ERL_NIF_TERM;
    pub fn enif_make_long(arg1: *mut ErlNifEnv, i: c_long) -> ERL_NIF_TERM;
    pub fn enif_make_tuple_from_array(arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint) -> ERL_NIF_TERM;
    pub fn enif_make_list_from_array(arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint) -> ERL_NIF_TERM;
    pub fn enif_is_empty_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_open_resource_type(arg1: *mut ErlNifEnv, module_str: *const c_char, name_str: *const c_char, dtor: Option<unsafe extern "C" fn (*mut ErlNifEnv, *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType;
    pub fn enif_alloc_resource(type_: *const ErlNifResourceType, size: size_t) -> *mut c_void;
    pub fn enif_release_resource(obj: *const c_void);
    pub fn enif_make_resource(arg1: *mut ErlNifEnv, obj: *const c_void) -> ERL_NIF_TERM;
    pub fn enif_get_resource(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, type_: *const ErlNifResourceType, objp: *mut *const c_void) -> c_int;
    pub fn enif_sizeof_resource(obj: *mut c_void) -> size_t;
    pub fn enif_make_new_binary(arg1: *mut ErlNifEnv, size: size_t, termp: *mut ERL_NIF_TERM) -> *mut c_uchar;
    pub fn enif_is_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_is_tuple(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_get_atom_length(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, len: *mut c_uint, arg4: ErlNifCharEncoding) -> c_int;
    pub fn enif_get_list_length(env: *mut ErlNifEnv, term: ERL_NIF_TERM, len: *mut c_uint) -> c_int;
    pub fn enif_make_atom_len(env: *mut ErlNifEnv, name: *const c_char, len: size_t) -> ERL_NIF_TERM;
    pub fn enif_make_existing_atom_len(env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, arg5: ErlNifCharEncoding) -> c_int;
    pub fn enif_make_string_len(env: *mut ErlNifEnv, string: *const c_char, len: size_t, arg4: ErlNifCharEncoding) -> ERL_NIF_TERM;
    pub fn enif_alloc_env() -> *mut ErlNifEnv;
    pub fn enif_free_env(env: *mut ErlNifEnv);
    pub fn enif_clear_env(env: *mut ErlNifEnv);
    pub fn enif_send(env: *mut ErlNifEnv, to_pid: *const ErlNifPid, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM) -> c_int;
    pub fn enif_make_copy(dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM) -> ERL_NIF_TERM;
    pub fn enif_self(caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid) -> *mut ErlNifPid;
    pub fn enif_get_local_pid(env: *mut ErlNifEnv, arg2: ERL_NIF_TERM, pid: *mut ErlNifPid) -> c_int;
    pub fn enif_keep_resource(obj: *const c_void);
    pub fn enif_make_resource_binary(arg1: *mut ErlNifEnv, obj: *const c_void, data: *const c_void, size: size_t) -> ERL_NIF_TERM;
    pub fn enif_is_exception(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_make_reverse_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, list: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_is_number(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_dlopen(lib: *const c_char, err_handler: Option<unsafe extern "C" fn (*mut c_void, *const c_char)>, err_arg: *mut c_void) -> *mut c_void;
    pub fn enif_dlsym(handle: *mut c_void, symbol: *const c_char, err_handler: Option<unsafe extern "C" fn (*mut c_void, *const c_char)>, err_arg: *mut c_void) -> *mut c_void;
    pub fn enif_consume_timeslice(arg1: *mut ErlNifEnv, percent: c_int) -> c_int;
    pub fn enif_is_map(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
    pub fn enif_get_map_size(env: *mut ErlNifEnv, term: ERL_NIF_TERM, size: *mut size_t) -> c_int;
    pub fn enif_make_new_map(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    pub fn enif_make_map_put(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_get_map_value(env: *mut ErlNifEnv, map: ERL_NIF_TERM, key: ERL_NIF_TERM, value: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_make_map_update(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_make_map_remove(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_map_iterator_create(env: *mut ErlNifEnv, map: ERL_NIF_TERM, iter: *mut ErlNifMapIterator, entry: ErlNifMapIteratorEntry) -> c_int;
    pub fn enif_map_iterator_destroy(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator);
    pub fn enif_map_iterator_is_head(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    pub fn enif_map_iterator_is_tail(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    pub fn enif_map_iterator_next(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    pub fn enif_map_iterator_prev(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int;
    pub fn enif_map_iterator_get_pair(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator, key: *mut ERL_NIF_TERM, value: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_schedule_nif(arg1: *mut ErlNifEnv, arg2: *const c_char, arg3: c_int, arg4: unsafe extern "C" fn(*mut ErlNifEnv, c_int, *const ERL_NIF_TERM) -> ERL_NIF_TERM, arg5: c_int, arg6: *const ERL_NIF_TERM) -> ERL_NIF_TERM;
    pub fn enif_has_pending_exception(env: *mut ErlNifEnv, reason: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_raise_exception(env: *mut ErlNifEnv, reason: ERL_NIF_TERM) -> ERL_NIF_TERM;
    pub fn enif_getenv(key: *const c_char, value: *mut c_char, value_size: *mut size_t) -> c_int;
    pub fn enif_monotonic_time(arg1: ErlNifTimeUnit) -> ErlNifTime;
    pub fn enif_time_offset(arg1: ErlNifTimeUnit) -> ErlNifTime;
    pub fn enif_convert_time_unit(arg1: ErlNifTime, arg2: ErlNifTimeUnit, arg3: ErlNifTimeUnit) -> ErlNifTime;
    pub fn enif_now_time(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    pub fn enif_cpu_time(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
    pub fn enif_make_unique_integer(env: *mut ErlNifEnv, properties: ErlNifUniqueInteger) -> ERL_NIF_TERM;
    pub fn enif_is_current_process_alive(env: *mut ErlNifEnv) -> c_int;
    pub fn enif_is_process_alive(env: *mut ErlNifEnv, pid: *const ErlNifPid) -> c_int;
    pub fn enif_is_port_alive(env: *mut ErlNifEnv, port_id: *mut ErlNifPort) -> c_int;
    pub fn enif_get_local_port(env: *mut ErlNifEnv, arg2: ERL_NIF_TERM, port_id: *mut ErlNifPort) -> c_int;
    pub fn enif_term_to_binary(env: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int;
    pub fn enif_binary_to_term(env: *mut ErlNifEnv, data: *const c_uchar, sz: size_t, term: *mut ERL_NIF_TERM, opts: c_uint) -> size_t;
    pub fn enif_port_command(env: *mut ErlNifEnv, to_port: *const ErlNifPort, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM) -> c_int;
    pub fn enif_thread_type() -> c_int;
    #[link_name = "enif_snprintf"]
    fn __variadic_enif_snprintf(buffer: *mut c_char, size: size_t, format: *const c_char, ...) -> c_int;
    pub fn enif_select(env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *mut c_void, pid: *const ErlNifPid, ref_: ERL_NIF_TERM) -> c_int;
    pub fn enif_open_resource_type_x(arg1: *mut ErlNifEnv, name_str: *const c_char, arg3: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType;
    pub fn enif_monitor_process(arg1: *mut ErlNifEnv, obj: *const c_void, arg3: *const ErlNifPid, monitor: *mut ErlNifMonitor) -> c_int;
    pub fn enif_demonitor_process(arg1: *mut ErlNifEnv, obj: *const c_void, monitor: *const ErlNifMonitor) -> c_int;
    pub fn enif_compare_monitors(arg1: *const ErlNifMonitor, arg2: *const ErlNifMonitor) -> c_int;
    pub fn enif_hash(type_: ErlNifHash, term: ERL_NIF_TERM, salt: u64) -> u64;
    pub fn enif_whereis_pid(env: *mut ErlNifEnv, name: ERL_NIF_TERM, pid: *mut ErlNifPid) -> c_int;
    pub fn enif_whereis_port(env: *mut ErlNifEnv, name: ERL_NIF_TERM, port: *mut ErlNifPort) -> c_int;
    pub fn enif_ioq_create(opts: ErlNifIOQueueOpts) -> *mut ErlNifIOQueue;
    pub fn enif_ioq_destroy(q: *mut ErlNifIOQueue);
    pub fn enif_ioq_enq_binary(q: *mut ErlNifIOQueue, bin: *mut ErlNifBinary, skip: size_t) -> c_int;
    pub fn enif_ioq_enqv(q: *mut ErlNifIOQueue, iov: *mut ErlNifIOVec, skip: size_t) -> c_int;
    pub fn enif_ioq_size(q: *mut ErlNifIOQueue) -> size_t;
    pub fn enif_ioq_deq(q: *mut ErlNifIOQueue, count: size_t, size: *mut size_t) -> c_int;
    pub fn enif_ioq_peek(q: *mut ErlNifIOQueue, iovlen: *mut c_int) -> *mut SysIOVec;
    pub fn enif_inspect_iovec(env: *mut ErlNifEnv, max_length: size_t, iovec_term: ERL_NIF_TERM, tail: *mut ERL_NIF_TERM, iovec: *mut *mut ErlNifIOVec) -> c_int;
    pub fn enif_free_iovec(iov: *mut ErlNifIOVec);
    pub fn enif_ioq_peek_head(env: *mut ErlNifEnv, q: *mut ErlNifIOQueue, size: *mut size_t, head: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_make_map_from_arrays(env: *mut ErlNifEnv, keys: *const ERL_NIF_TERM, values: *const ERL_NIF_TERM, cnt: size_t, map_out: *mut ERL_NIF_TERM) -> c_int;
    pub fn enif_select_x(env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *mut c_void, pid: *const ErlNifPid, msg: ERL_NIF_TERM, msg_env: *mut ErlNifEnv) -> c_int;
    pub fn enif_make_monitor_term(env: *mut ErlNifEnv, arg2: *const ErlNifMonitor) -> ERL_NIF_TERM;
    pub fn enif_set_pid_undefined(pid: *mut ErlNifPid);
    pub fn enif_is_pid_undefined(pid: *const ErlNifPid) -> c_int;
    pub fn enif_term_type(env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> ErlNifTermType;
    pub fn enif_init_resource_type(arg1: *mut ErlNifEnv, name_str: *const c_char, arg3: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType;
    pub fn enif_dynamic_resource_call(arg1: *mut ErlNifEnv, mod_: ERL_NIF_TERM, name: ERL_NIF_TERM, rsrc: ERL_NIF_TERM, call_data: *mut c_void) -> c_int;
}

#[macro_export] macro_rules! enif_make_tuple {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_make_tuple()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_make_tuple!($($arg),*) };
}

pub use enif_make_tuple;

pub unsafe fn get_enif_make_tuple() -> extern "C" fn (env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM {
    std::mem::transmute(__variadic_enif_make_tuple as *const ())
}

#[macro_export] macro_rules! enif_make_list {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_make_list()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_make_list!($($arg),*) };
}

pub use enif_make_list;

pub unsafe fn get_enif_make_list() -> extern "C" fn (env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM {
    std::mem::transmute(__variadic_enif_make_list as *const ())
}

#[macro_export] macro_rules! enif_fprintf {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_fprintf()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_fprintf!($($arg),*) };
}

pub use enif_fprintf;

pub unsafe fn get_enif_fprintf() -> extern "C" fn (filep: *mut c_void, format: *const c_char, ...) -> c_int {
    std::mem::transmute(__variadic_enif_fprintf as *const ())
}

#[macro_export] macro_rules! enif_snprintf {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_snprintf()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_snprintf!($($arg),*) };
}

pub use enif_snprintf;

pub unsafe fn get_enif_snprintf() -> extern "C" fn (buffer: *mut c_char, size: size_t, format: *const c_char, ...) -> c_int {
    std::mem::transmute(__variadic_enif_snprintf as *const ())
}


/// See [enif_make_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int64) at erlang.org
#[inline]
pub unsafe fn enif_make_int64(env: *mut ErlNifEnv, i: i64) -> ERL_NIF_TERM
    { enif_make_long(env, i) }

/// See [enif_make_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint64) at erlang.org
#[inline]
pub unsafe fn enif_make_uint64(env: *mut ErlNifEnv, i: u64) -> ERL_NIF_TERM
    { enif_make_ulong(env, i) }

/// See [enif_get_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int64) at erlang.org
#[inline]
pub unsafe fn enif_get_int64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int
    { enif_get_long(env, term, ip) }

/// See [enif_get_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint64) at erlang.org
#[inline]
pub unsafe fn enif_get_uint64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int
    { enif_get_ulong(env, term, ip) }
        