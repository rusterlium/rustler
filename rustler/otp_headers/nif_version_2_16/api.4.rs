pub const ERL_NIF_ENTRY_OPTIONS: c_uint = ERL_NIF_DIRTY_NIF_OPTION;
#[allow(dead_code)]
#[derive(Default, Copy, Clone)]
pub struct DynNifCallbacks {
    enif_priv_data: Option<extern "C" fn (arg1: *mut ErlNifEnv) -> *mut c_void>,
    enif_alloc: Option<extern "C" fn (size: size_t) -> *mut c_void>,
    enif_free: Option<extern "C" fn (ptr: *mut c_void)>,
    enif_is_atom: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_is_binary: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_is_ref: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_inspect_binary: Option<extern "C" fn (arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int>,
    enif_alloc_binary: Option<extern "C" fn (size: size_t, bin: *mut ErlNifBinary) -> c_int>,
    enif_realloc_binary: Option<extern "C" fn (bin: *mut ErlNifBinary, size: size_t) -> c_int>,
    enif_release_binary: Option<extern "C" fn (bin: *mut ErlNifBinary)>,
    enif_get_int: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_int) -> c_int>,
    enif_get_ulong: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulong) -> c_int>,
    enif_get_double: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, dp: *mut c_double) -> c_int>,
    enif_get_list_cell: Option<extern "C" fn (env: *mut ErlNifEnv, term: ERL_NIF_TERM, head: *mut ERL_NIF_TERM, tail: *mut ERL_NIF_TERM) -> c_int>,
    enif_get_tuple: Option<extern "C" fn (env: *mut ErlNifEnv, tpl: ERL_NIF_TERM, arity: *mut c_int, array: *mut *const ERL_NIF_TERM) -> c_int>,
    enif_is_identical: Option<extern "C" fn (lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c_int>,
    enif_compare: Option<extern "C" fn (lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) -> c_int>,
    enif_make_binary: Option<extern "C" fn (env: *mut ErlNifEnv, bin: *mut ErlNifBinary) -> ERL_NIF_TERM>,
    enif_make_badarg: Option<extern "C" fn (env: *mut ErlNifEnv) -> ERL_NIF_TERM>,
    enif_make_int: Option<extern "C" fn (env: *mut ErlNifEnv, i: c_int) -> ERL_NIF_TERM>,
    enif_make_ulong: Option<extern "C" fn (env: *mut ErlNifEnv, i: c_ulong) -> ERL_NIF_TERM>,
    enif_make_double: Option<extern "C" fn (env: *mut ErlNifEnv, d: c_double) -> ERL_NIF_TERM>,
    enif_make_atom: Option<extern "C" fn (env: *mut ErlNifEnv, name: *const c_char) -> ERL_NIF_TERM>,
    enif_make_existing_atom: Option<extern "C" fn (env: *mut ErlNifEnv, name: *const c_char, atom: *mut ERL_NIF_TERM, arg4: ErlNifCharEncoding) -> c_int>,
    enif_make_tuple: Option<extern "C" fn (env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM>,
    enif_make_list: Option<extern "C" fn (env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM>,
    enif_make_list_cell: Option<extern "C" fn (env: *mut ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM) -> ERL_NIF_TERM>,
    enif_make_string: Option<extern "C" fn (env: *mut ErlNifEnv, string: *const c_char, arg3: ErlNifCharEncoding) -> ERL_NIF_TERM>,
    enif_make_ref: Option<extern "C" fn (env: *mut ErlNifEnv) -> ERL_NIF_TERM>,
    enif_mutex_create: Option<extern "C" fn ()>,
    enif_mutex_destroy: Option<extern "C" fn ()>,
    enif_mutex_trylock: Option<extern "C" fn ()>,
    enif_mutex_lock: Option<extern "C" fn ()>,
    enif_mutex_unlock: Option<extern "C" fn ()>,
    enif_cond_create: Option<extern "C" fn ()>,
    enif_cond_destroy: Option<extern "C" fn ()>,
    enif_cond_signal: Option<extern "C" fn ()>,
    enif_cond_broadcast: Option<extern "C" fn ()>,
    enif_cond_wait: Option<extern "C" fn ()>,
    enif_rwlock_create: Option<extern "C" fn ()>,
    enif_rwlock_destroy: Option<extern "C" fn ()>,
    enif_rwlock_tryrlock: Option<extern "C" fn ()>,
    enif_rwlock_rlock: Option<extern "C" fn ()>,
    enif_rwlock_runlock: Option<extern "C" fn ()>,
    enif_rwlock_tryrwlock: Option<extern "C" fn ()>,
    enif_rwlock_rwlock: Option<extern "C" fn ()>,
    enif_rwlock_rwunlock: Option<extern "C" fn ()>,
    enif_tsd_key_create: Option<extern "C" fn ()>,
    enif_tsd_key_destroy: Option<extern "C" fn ()>,
    enif_tsd_set: Option<extern "C" fn ()>,
    enif_tsd_get: Option<extern "C" fn ()>,
    enif_thread_opts_create: Option<extern "C" fn ()>,
    enif_thread_opts_destroy: Option<extern "C" fn ()>,
    enif_thread_create: Option<extern "C" fn ()>,
    enif_thread_self: Option<extern "C" fn ()>,
    enif_equal_tids: Option<extern "C" fn ()>,
    enif_thread_exit: Option<extern "C" fn ()>,
    enif_thread_join: Option<extern "C" fn ()>,
    enif_realloc: Option<extern "C" fn (ptr: *mut c_void, size: size_t) -> *mut c_void>,
    enif_system_info: Option<extern "C" fn (sip: *mut ErlNifSysInfo, si_size: size_t)>,
    enif_fprintf: Option<extern "C" fn (filep: *mut c_void, format: *const c_char, ...) -> c_int>,
    enif_inspect_iolist_as_binary: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int>,
    enif_make_sub_binary: Option<extern "C" fn (arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, pos: size_t, size: size_t) -> ERL_NIF_TERM>,
    enif_get_string: Option<extern "C" fn (arg1: *mut ErlNifEnv, list: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg5: ErlNifCharEncoding) -> c_int>,
    enif_get_atom: Option<extern "C" fn (arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg5: ErlNifCharEncoding) -> c_int>,
    enif_is_fun: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_is_pid: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_is_port: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_get_uint: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_uint) -> c_int>,
    enif_get_long: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_long) -> c_int>,
    enif_make_uint: Option<extern "C" fn (arg1: *mut ErlNifEnv, i: c_uint) -> ERL_NIF_TERM>,
    enif_make_long: Option<extern "C" fn (arg1: *mut ErlNifEnv, i: c_long) -> ERL_NIF_TERM>,
    enif_make_tuple_from_array: Option<extern "C" fn (arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint) -> ERL_NIF_TERM>,
    enif_make_list_from_array: Option<extern "C" fn (arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint) -> ERL_NIF_TERM>,
    enif_is_empty_list: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_open_resource_type: Option<extern "C" fn (arg1: *mut ErlNifEnv, module_str: *const c_char, name_str: *const c_char, dtor: Option<unsafe extern "C" fn (*mut ErlNifEnv, *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType>,
    enif_alloc_resource: Option<extern "C" fn (type_: *const ErlNifResourceType, size: size_t) -> *mut c_void>,
    enif_release_resource: Option<extern "C" fn (obj: *const c_void)>,
    enif_make_resource: Option<extern "C" fn (arg1: *mut ErlNifEnv, obj: *const c_void) -> ERL_NIF_TERM>,
    enif_get_resource: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, type_: *const ErlNifResourceType, objp: *mut *const c_void) -> c_int>,
    enif_sizeof_resource: Option<extern "C" fn (obj: *mut c_void) -> size_t>,
    enif_make_new_binary: Option<extern "C" fn (arg1: *mut ErlNifEnv, size: size_t, termp: *mut ERL_NIF_TERM) -> *mut c_uchar>,
    enif_is_list: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_is_tuple: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_get_atom_length: Option<extern "C" fn (arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, len: *mut c_uint, arg4: ErlNifCharEncoding) -> c_int>,
    enif_get_list_length: Option<extern "C" fn (env: *mut ErlNifEnv, term: ERL_NIF_TERM, len: *mut c_uint) -> c_int>,
    enif_make_atom_len: Option<extern "C" fn (env: *mut ErlNifEnv, name: *const c_char, len: size_t) -> ERL_NIF_TERM>,
    enif_make_existing_atom_len: Option<extern "C" fn (env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, arg5: ErlNifCharEncoding) -> c_int>,
    enif_make_string_len: Option<extern "C" fn (env: *mut ErlNifEnv, string: *const c_char, len: size_t, arg4: ErlNifCharEncoding) -> ERL_NIF_TERM>,
    enif_alloc_env: Option<extern "C" fn () -> *mut ErlNifEnv>,
    enif_free_env: Option<extern "C" fn (env: *mut ErlNifEnv)>,
    enif_clear_env: Option<extern "C" fn (env: *mut ErlNifEnv)>,
    enif_send: Option<extern "C" fn (env: *mut ErlNifEnv, to_pid: *const ErlNifPid, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM) -> c_int>,
    enif_make_copy: Option<extern "C" fn (dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM) -> ERL_NIF_TERM>,
    enif_self: Option<extern "C" fn (caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid) -> *mut ErlNifPid>,
    enif_get_local_pid: Option<extern "C" fn (env: *mut ErlNifEnv, arg2: ERL_NIF_TERM, pid: *mut ErlNifPid) -> c_int>,
    enif_keep_resource: Option<extern "C" fn (obj: *const c_void)>,
    enif_make_resource_binary: Option<extern "C" fn (arg1: *mut ErlNifEnv, obj: *const c_void, data: *const c_void, size: size_t) -> ERL_NIF_TERM>,
    enif_get_int64: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int>,
    enif_get_uint64: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int>,
    enif_make_int64: Option<extern "C" fn (arg1: *mut ErlNifEnv, arg2: i64) -> ERL_NIF_TERM>,
    enif_make_uint64: Option<extern "C" fn (arg1: *mut ErlNifEnv, arg2: u64) -> ERL_NIF_TERM>,
    enif_is_exception: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_make_reverse_list: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, list: *mut ERL_NIF_TERM) -> c_int>,
    enif_is_number: Option<extern "C" fn (arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_dlopen: Option<extern "C" fn (lib: *const c_char, err_handler: Option<unsafe extern "C" fn (*mut c_void, *const c_char)>, err_arg: *mut c_void) -> *mut c_void>,
    enif_dlsym: Option<extern "C" fn (handle: *mut c_void, symbol: *const c_char, err_handler: Option<unsafe extern "C" fn (*mut c_void, *const c_char)>, err_arg: *mut c_void) -> *mut c_void>,
    enif_consume_timeslice: Option<extern "C" fn (arg1: *mut ErlNifEnv, percent: c_int) -> c_int>,
    enif_is_map: Option<extern "C" fn (env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int>,
    enif_get_map_size: Option<extern "C" fn (env: *mut ErlNifEnv, term: ERL_NIF_TERM, size: *mut size_t) -> c_int>,
    enif_make_new_map: Option<extern "C" fn (env: *mut ErlNifEnv) -> ERL_NIF_TERM>,
    enif_make_map_put: Option<extern "C" fn (env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int>,
    enif_get_map_value: Option<extern "C" fn (env: *mut ErlNifEnv, map: ERL_NIF_TERM, key: ERL_NIF_TERM, value: *mut ERL_NIF_TERM) -> c_int>,
    enif_make_map_update: Option<extern "C" fn (env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int>,
    enif_make_map_remove: Option<extern "C" fn (env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM) -> c_int>,
    enif_map_iterator_create: Option<extern "C" fn (env: *mut ErlNifEnv, map: ERL_NIF_TERM, iter: *mut ErlNifMapIterator, entry: ErlNifMapIteratorEntry) -> c_int>,
    enif_map_iterator_destroy: Option<extern "C" fn (env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator)>,
    enif_map_iterator_is_head: Option<extern "C" fn (env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int>,
    enif_map_iterator_is_tail: Option<extern "C" fn (env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int>,
    enif_map_iterator_next: Option<extern "C" fn (env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int>,
    enif_map_iterator_prev: Option<extern "C" fn (env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator) -> c_int>,
    enif_map_iterator_get_pair: Option<extern "C" fn (env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator, key: *mut ERL_NIF_TERM, value: *mut ERL_NIF_TERM) -> c_int>,
    enif_schedule_nif: Option<extern "C" fn (arg1: *mut ErlNifEnv, arg2: *const c_char, arg3: c_int, arg4: unsafe extern "C" fn(*mut ErlNifEnv, c_int, *const ERL_NIF_TERM) -> ERL_NIF_TERM, arg5: c_int, arg6: *const ERL_NIF_TERM) -> ERL_NIF_TERM>,
    enif_has_pending_exception: Option<extern "C" fn (env: *mut ErlNifEnv, reason: *mut ERL_NIF_TERM) -> c_int>,
    enif_raise_exception: Option<extern "C" fn (env: *mut ErlNifEnv, reason: ERL_NIF_TERM) -> ERL_NIF_TERM>,
    enif_getenv: Option<extern "C" fn (key: *const c_char, value: *mut c_char, value_size: *mut size_t) -> c_int>,
    enif_monotonic_time: Option<extern "C" fn (arg1: ErlNifTimeUnit) -> ErlNifTime>,
    enif_time_offset: Option<extern "C" fn (arg1: ErlNifTimeUnit) -> ErlNifTime>,
    enif_convert_time_unit: Option<extern "C" fn (arg1: ErlNifTime, arg2: ErlNifTimeUnit, arg3: ErlNifTimeUnit) -> ErlNifTime>,
    enif_now_time: Option<extern "C" fn (env: *mut ErlNifEnv) -> ERL_NIF_TERM>,
    enif_cpu_time: Option<extern "C" fn (env: *mut ErlNifEnv) -> ERL_NIF_TERM>,
    enif_make_unique_integer: Option<extern "C" fn (env: *mut ErlNifEnv, properties: ErlNifUniqueInteger) -> ERL_NIF_TERM>,
    enif_is_current_process_alive: Option<extern "C" fn (env: *mut ErlNifEnv) -> c_int>,
    enif_is_process_alive: Option<extern "C" fn (env: *mut ErlNifEnv, pid: *const ErlNifPid) -> c_int>,
    enif_is_port_alive: Option<extern "C" fn (env: *mut ErlNifEnv, port_id: *mut ErlNifPort) -> c_int>,
    enif_get_local_port: Option<extern "C" fn (env: *mut ErlNifEnv, arg2: ERL_NIF_TERM, port_id: *mut ErlNifPort) -> c_int>,
    enif_term_to_binary: Option<extern "C" fn (env: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary) -> c_int>,
    enif_binary_to_term: Option<extern "C" fn (env: *mut ErlNifEnv, data: *const c_uchar, sz: size_t, term: *mut ERL_NIF_TERM, opts: c_uint) -> size_t>,
    enif_port_command: Option<extern "C" fn (env: *mut ErlNifEnv, to_port: *const ErlNifPort, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM) -> c_int>,
    enif_thread_type: Option<extern "C" fn () -> c_int>,
    enif_snprintf: Option<extern "C" fn (buffer: *mut c_char, size: size_t, format: *const c_char, ...) -> c_int>,
    enif_select: Option<extern "C" fn (env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *mut c_void, pid: *const ErlNifPid, ref_: ERL_NIF_TERM) -> c_int>,
    enif_open_resource_type_x: Option<extern "C" fn (arg1: *mut ErlNifEnv, name_str: *const c_char, arg3: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType>,
    enif_monitor_process: Option<extern "C" fn (arg1: *mut ErlNifEnv, obj: *const c_void, arg3: *const ErlNifPid, monitor: *mut ErlNifMonitor) -> c_int>,
    enif_demonitor_process: Option<extern "C" fn (arg1: *mut ErlNifEnv, obj: *const c_void, monitor: *const ErlNifMonitor) -> c_int>,
    enif_compare_monitors: Option<extern "C" fn (arg1: *const ErlNifMonitor, arg2: *const ErlNifMonitor) -> c_int>,
    enif_hash: Option<extern "C" fn (type_: ErlNifHash, term: ERL_NIF_TERM, salt: u64) -> u64>,
    enif_whereis_pid: Option<extern "C" fn (env: *mut ErlNifEnv, name: ERL_NIF_TERM, pid: *mut ErlNifPid) -> c_int>,
    enif_whereis_port: Option<extern "C" fn (env: *mut ErlNifEnv, name: ERL_NIF_TERM, port: *mut ErlNifPort) -> c_int>,
    enif_ioq_create: Option<extern "C" fn (opts: ErlNifIOQueueOpts) -> *mut ErlNifIOQueue>,
    enif_ioq_destroy: Option<extern "C" fn (q: *mut ErlNifIOQueue)>,
    enif_ioq_enq_binary: Option<extern "C" fn (q: *mut ErlNifIOQueue, bin: *mut ErlNifBinary, skip: size_t) -> c_int>,
    enif_ioq_enqv: Option<extern "C" fn (q: *mut ErlNifIOQueue, iov: *mut ErlNifIOVec, skip: size_t) -> c_int>,
    enif_ioq_size: Option<extern "C" fn (q: *mut ErlNifIOQueue) -> size_t>,
    enif_ioq_deq: Option<extern "C" fn (q: *mut ErlNifIOQueue, count: size_t, size: *mut size_t) -> c_int>,
    enif_ioq_peek: Option<extern "C" fn (q: *mut ErlNifIOQueue, iovlen: *mut c_int) -> *mut SysIOVec>,
    enif_inspect_iovec: Option<extern "C" fn (env: *mut ErlNifEnv, max_length: size_t, iovec_term: ERL_NIF_TERM, tail: *mut ERL_NIF_TERM, iovec: *mut *mut ErlNifIOVec) -> c_int>,
    enif_free_iovec: Option<extern "C" fn (iov: *mut ErlNifIOVec)>,
    enif_ioq_peek_head: Option<extern "C" fn (env: *mut ErlNifEnv, q: *mut ErlNifIOQueue, size: *mut size_t, head: *mut ERL_NIF_TERM) -> c_int>,
    enif_mutex_name: Option<extern "C" fn ()>,
    enif_cond_name: Option<extern "C" fn ()>,
    enif_rwlock_name: Option<extern "C" fn ()>,
    enif_thread_name: Option<extern "C" fn ()>,
    enif_vfprintf: Option<extern "C" fn ()>,
    enif_vsnprintf: Option<extern "C" fn ()>,
    enif_make_map_from_arrays: Option<extern "C" fn (env: *mut ErlNifEnv, keys: *const ERL_NIF_TERM, values: *const ERL_NIF_TERM, cnt: size_t, map_out: *mut ERL_NIF_TERM) -> c_int>,
    enif_select_x: Option<extern "C" fn (env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *mut c_void, pid: *const ErlNifPid, msg: ERL_NIF_TERM, msg_env: *mut ErlNifEnv) -> c_int>,
    enif_make_monitor_term: Option<extern "C" fn (env: *mut ErlNifEnv, arg2: *const ErlNifMonitor) -> ERL_NIF_TERM>,
    enif_set_pid_undefined: Option<extern "C" fn (pid: *mut ErlNifPid)>,
    enif_is_pid_undefined: Option<extern "C" fn (pid: *const ErlNifPid) -> c_int>,
    enif_term_type: Option<extern "C" fn (env: *mut ErlNifEnv, term: ERL_NIF_TERM) -> ErlNifTermType>,
    enif_init_resource_type: Option<extern "C" fn (arg1: *mut ErlNifEnv, name_str: *const c_char, arg3: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags) -> *const ErlNifResourceType>,
    enif_dynamic_resource_call: Option<extern "C" fn (arg1: *mut ErlNifEnv, mod_: ERL_NIF_TERM, name: ERL_NIF_TERM, rsrc: ERL_NIF_TERM, call_data: *mut c_void) -> c_int>,
}
/// See [enif_priv_data](http://www.erlang.org/doc/man/erl_nif.html#enif_priv_data) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_priv_data(arg1: *mut ErlNifEnv)
 -> *mut c_void{
    (DYN_NIF_CALLBACKS.enif_priv_data.unwrap_unchecked())(arg1)
}

/// See [enif_alloc](http://www.erlang.org/doc/man/erl_nif.html#enif_alloc) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_alloc(size: size_t)
 -> *mut c_void{
    (DYN_NIF_CALLBACKS.enif_alloc.unwrap_unchecked())(size)
}

/// See [enif_free](http://www.erlang.org/doc/man/erl_nif.html#enif_free) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_free(ptr: *mut c_void)
{
    (DYN_NIF_CALLBACKS.enif_free.unwrap_unchecked())(ptr)
}

/// See [enif_is_atom](http://www.erlang.org/doc/man/erl_nif.html#enif_is_atom) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_atom(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_atom.unwrap_unchecked())(arg1, term)
}

/// See [enif_is_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_is_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_binary.unwrap_unchecked())(arg1, term)
}

/// See [enif_is_ref](http://www.erlang.org/doc/man/erl_nif.html#enif_is_ref) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_ref(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_ref.unwrap_unchecked())(arg1, term)
}

/// See [enif_inspect_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_inspect_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_inspect_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, bin: *mut ErlNifBinary)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_inspect_binary.unwrap_unchecked())(arg1, bin_term, bin)
}

/// See [enif_alloc_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_alloc_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_alloc_binary(size: size_t, bin: *mut ErlNifBinary)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_alloc_binary.unwrap_unchecked())(size, bin)
}

/// See [enif_realloc_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_realloc_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_realloc_binary(bin: *mut ErlNifBinary, size: size_t)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_realloc_binary.unwrap_unchecked())(bin, size)
}

/// See [enif_release_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_release_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_release_binary(bin: *mut ErlNifBinary)
{
    (DYN_NIF_CALLBACKS.enif_release_binary.unwrap_unchecked())(bin)
}

/// See [enif_get_int](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_int(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_int)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_int.unwrap_unchecked())(arg1, term, ip)
}

/// See [enif_get_ulong](http://www.erlang.org/doc/man/erl_nif.html#enif_get_ulong) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_ulong(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulong)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_ulong.unwrap_unchecked())(arg1, term, ip)
}

/// See [enif_get_double](http://www.erlang.org/doc/man/erl_nif.html#enif_get_double) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_double(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, dp: *mut c_double)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_double.unwrap_unchecked())(arg1, term, dp)
}

/// See [enif_get_list_cell](http://www.erlang.org/doc/man/erl_nif.html#enif_get_list_cell) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_list_cell(env: *mut ErlNifEnv, term: ERL_NIF_TERM, head: *mut ERL_NIF_TERM, tail: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_list_cell.unwrap_unchecked())(env, term, head, tail)
}

/// See [enif_get_tuple](http://www.erlang.org/doc/man/erl_nif.html#enif_get_tuple) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_tuple(env: *mut ErlNifEnv, tpl: ERL_NIF_TERM, arity: *mut c_int, array: *mut *const ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_tuple.unwrap_unchecked())(env, tpl, arity, array)
}

/// See [enif_is_identical](http://www.erlang.org/doc/man/erl_nif.html#enif_is_identical) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_identical(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_identical.unwrap_unchecked())(lhs, rhs)
}

/// See [enif_compare](http://www.erlang.org/doc/man/erl_nif.html#enif_compare) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_compare(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_compare.unwrap_unchecked())(lhs, rhs)
}

/// See [enif_make_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_make_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_binary(env: *mut ErlNifEnv, bin: *mut ErlNifBinary)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_binary.unwrap_unchecked())(env, bin)
}

/// See [enif_make_badarg](http://www.erlang.org/doc/man/erl_nif.html#enif_make_badarg) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_badarg(env: *mut ErlNifEnv)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_badarg.unwrap_unchecked())(env)
}

/// See [enif_make_int](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_int(env: *mut ErlNifEnv, i: c_int)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_int.unwrap_unchecked())(env, i)
}

/// See [enif_make_ulong](http://www.erlang.org/doc/man/erl_nif.html#enif_make_ulong) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_ulong(env: *mut ErlNifEnv, i: c_ulong)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_ulong.unwrap_unchecked())(env, i)
}

/// See [enif_make_double](http://www.erlang.org/doc/man/erl_nif.html#enif_make_double) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_double(env: *mut ErlNifEnv, d: c_double)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_double.unwrap_unchecked())(env, d)
}

/// See [enif_make_atom](http://www.erlang.org/doc/man/erl_nif.html#enif_make_atom) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_atom(env: *mut ErlNifEnv, name: *const c_char)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_atom.unwrap_unchecked())(env, name)
}

/// See [enif_make_existing_atom](http://www.erlang.org/doc/man/erl_nif.html#enif_make_existing_atom) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_existing_atom(env: *mut ErlNifEnv, name: *const c_char, atom: *mut ERL_NIF_TERM, arg4: ErlNifCharEncoding)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_make_existing_atom.unwrap_unchecked())(env, name, atom, arg4)
}

#[macro_export] macro_rules! enif_make_tuple {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_make_tuple()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_make_tuple!($($arg),*) };
}

pub use enif_make_tuple;

pub unsafe fn get_enif_make_tuple() -> extern "C" fn (env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM {
    DYN_NIF_CALLBACKS.enif_make_tuple.unwrap_unchecked()
}

#[macro_export] macro_rules! enif_make_list {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_make_list()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_make_list!($($arg),*) };
}

pub use enif_make_list;

pub unsafe fn get_enif_make_list() -> extern "C" fn (env: *mut ErlNifEnv, cnt: c_uint, ...) -> ERL_NIF_TERM {
    DYN_NIF_CALLBACKS.enif_make_list.unwrap_unchecked()
}

/// See [enif_make_list_cell](http://www.erlang.org/doc/man/erl_nif.html#enif_make_list_cell) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_list_cell(env: *mut ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_list_cell.unwrap_unchecked())(env, car, cdr)
}

/// See [enif_make_string](http://www.erlang.org/doc/man/erl_nif.html#enif_make_string) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_string(env: *mut ErlNifEnv, string: *const c_char, arg3: ErlNifCharEncoding)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_string.unwrap_unchecked())(env, string, arg3)
}

/// See [enif_make_ref](http://www.erlang.org/doc/man/erl_nif.html#enif_make_ref) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_ref(env: *mut ErlNifEnv)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_ref.unwrap_unchecked())(env)
}

/// See [enif_realloc](http://www.erlang.org/doc/man/erl_nif.html#enif_realloc) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_realloc(ptr: *mut c_void, size: size_t)
 -> *mut c_void{
    (DYN_NIF_CALLBACKS.enif_realloc.unwrap_unchecked())(ptr, size)
}

/// See [enif_system_info](http://www.erlang.org/doc/man/erl_nif.html#enif_system_info) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_system_info(sip: *mut ErlNifSysInfo, si_size: size_t)
{
    (DYN_NIF_CALLBACKS.enif_system_info.unwrap_unchecked())(sip, si_size)
}

#[macro_export] macro_rules! enif_fprintf {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_fprintf()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_fprintf!($($arg),*) };
}

pub use enif_fprintf;

pub unsafe fn get_enif_fprintf() -> extern "C" fn (filep: *mut c_void, format: *const c_char, ...) -> c_int {
    DYN_NIF_CALLBACKS.enif_fprintf.unwrap_unchecked()
}

/// See [enif_inspect_iolist_as_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_inspect_iolist_as_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_inspect_iolist_as_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_inspect_iolist_as_binary.unwrap_unchecked())(arg1, term, bin)
}

/// See [enif_make_sub_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_make_sub_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_sub_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, pos: size_t, size: size_t)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_sub_binary.unwrap_unchecked())(arg1, bin_term, pos, size)
}

/// See [enif_get_string](http://www.erlang.org/doc/man/erl_nif.html#enif_get_string) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_string(arg1: *mut ErlNifEnv, list: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg5: ErlNifCharEncoding)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_string.unwrap_unchecked())(arg1, list, buf, len, arg5)
}

/// See [enif_get_atom](http://www.erlang.org/doc/man/erl_nif.html#enif_get_atom) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_atom(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg5: ErlNifCharEncoding)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_atom.unwrap_unchecked())(arg1, atom, buf, len, arg5)
}

/// See [enif_is_fun](http://www.erlang.org/doc/man/erl_nif.html#enif_is_fun) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_fun(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_fun.unwrap_unchecked())(arg1, term)
}

/// See [enif_is_pid](http://www.erlang.org/doc/man/erl_nif.html#enif_is_pid) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_pid(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_pid.unwrap_unchecked())(arg1, term)
}

/// See [enif_is_port](http://www.erlang.org/doc/man/erl_nif.html#enif_is_port) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_port(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_port.unwrap_unchecked())(arg1, term)
}

/// See [enif_get_uint](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_uint(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_uint)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_uint.unwrap_unchecked())(arg1, term, ip)
}

/// See [enif_get_long](http://www.erlang.org/doc/man/erl_nif.html#enif_get_long) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_long(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_long)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_long.unwrap_unchecked())(arg1, term, ip)
}

/// See [enif_make_uint](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_uint(arg1: *mut ErlNifEnv, i: c_uint)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_uint.unwrap_unchecked())(arg1, i)
}

/// See [enif_make_long](http://www.erlang.org/doc/man/erl_nif.html#enif_make_long) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_long(arg1: *mut ErlNifEnv, i: c_long)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_long.unwrap_unchecked())(arg1, i)
}

/// See [enif_make_tuple_from_array](http://www.erlang.org/doc/man/erl_nif.html#enif_make_tuple_from_array) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_tuple_from_array(arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_tuple_from_array.unwrap_unchecked())(arg1, arr, cnt)
}

/// See [enif_make_list_from_array](http://www.erlang.org/doc/man/erl_nif.html#enif_make_list_from_array) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_list_from_array(arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_list_from_array.unwrap_unchecked())(arg1, arr, cnt)
}

/// See [enif_is_empty_list](http://www.erlang.org/doc/man/erl_nif.html#enif_is_empty_list) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_empty_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_empty_list.unwrap_unchecked())(arg1, term)
}

/// See [enif_open_resource_type](http://www.erlang.org/doc/man/erl_nif.html#enif_open_resource_type) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_open_resource_type(arg1: *mut ErlNifEnv, module_str: *const c_char, name_str: *const c_char, dtor: Option<unsafe extern "C" fn (*mut ErlNifEnv, *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags)
 -> *const ErlNifResourceType{
    (DYN_NIF_CALLBACKS.enif_open_resource_type.unwrap_unchecked())(arg1, module_str, name_str, dtor, flags, tried)
}

/// See [enif_alloc_resource](http://www.erlang.org/doc/man/erl_nif.html#enif_alloc_resource) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_alloc_resource(type_: *const ErlNifResourceType, size: size_t)
 -> *mut c_void{
    (DYN_NIF_CALLBACKS.enif_alloc_resource.unwrap_unchecked())(type_, size)
}

/// See [enif_release_resource](http://www.erlang.org/doc/man/erl_nif.html#enif_release_resource) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_release_resource(obj: *const c_void)
{
    (DYN_NIF_CALLBACKS.enif_release_resource.unwrap_unchecked())(obj)
}

/// See [enif_make_resource](http://www.erlang.org/doc/man/erl_nif.html#enif_make_resource) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_resource(arg1: *mut ErlNifEnv, obj: *const c_void)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_resource.unwrap_unchecked())(arg1, obj)
}

/// See [enif_get_resource](http://www.erlang.org/doc/man/erl_nif.html#enif_get_resource) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_resource(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, type_: *const ErlNifResourceType, objp: *mut *const c_void)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_resource.unwrap_unchecked())(arg1, term, type_, objp)
}

/// See [enif_sizeof_resource](http://www.erlang.org/doc/man/erl_nif.html#enif_sizeof_resource) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_sizeof_resource(obj: *mut c_void)
 -> size_t{
    (DYN_NIF_CALLBACKS.enif_sizeof_resource.unwrap_unchecked())(obj)
}

/// See [enif_make_new_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_make_new_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_new_binary(arg1: *mut ErlNifEnv, size: size_t, termp: *mut ERL_NIF_TERM)
 -> *mut c_uchar{
    (DYN_NIF_CALLBACKS.enif_make_new_binary.unwrap_unchecked())(arg1, size, termp)
}

/// See [enif_is_list](http://www.erlang.org/doc/man/erl_nif.html#enif_is_list) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_list.unwrap_unchecked())(arg1, term)
}

/// See [enif_is_tuple](http://www.erlang.org/doc/man/erl_nif.html#enif_is_tuple) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_tuple(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_tuple.unwrap_unchecked())(arg1, term)
}

/// See [enif_get_atom_length](http://www.erlang.org/doc/man/erl_nif.html#enif_get_atom_length) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_atom_length(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, len: *mut c_uint, arg4: ErlNifCharEncoding)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_atom_length.unwrap_unchecked())(arg1, atom, len, arg4)
}

/// See [enif_get_list_length](http://www.erlang.org/doc/man/erl_nif.html#enif_get_list_length) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_list_length(env: *mut ErlNifEnv, term: ERL_NIF_TERM, len: *mut c_uint)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_list_length.unwrap_unchecked())(env, term, len)
}

/// See [enif_make_atom_len](http://www.erlang.org/doc/man/erl_nif.html#enif_make_atom_len) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_atom_len(env: *mut ErlNifEnv, name: *const c_char, len: size_t)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_atom_len.unwrap_unchecked())(env, name, len)
}

/// See [enif_make_existing_atom_len](http://www.erlang.org/doc/man/erl_nif.html#enif_make_existing_atom_len) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_existing_atom_len(env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, arg5: ErlNifCharEncoding)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_make_existing_atom_len.unwrap_unchecked())(env, name, len, atom, arg5)
}

/// See [enif_make_string_len](http://www.erlang.org/doc/man/erl_nif.html#enif_make_string_len) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_string_len(env: *mut ErlNifEnv, string: *const c_char, len: size_t, arg4: ErlNifCharEncoding)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_string_len.unwrap_unchecked())(env, string, len, arg4)
}

/// See [enif_alloc_env](http://www.erlang.org/doc/man/erl_nif.html#enif_alloc_env) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_alloc_env()
 -> *mut ErlNifEnv{
    (DYN_NIF_CALLBACKS.enif_alloc_env.unwrap_unchecked())()
}

/// See [enif_free_env](http://www.erlang.org/doc/man/erl_nif.html#enif_free_env) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_free_env(env: *mut ErlNifEnv)
{
    (DYN_NIF_CALLBACKS.enif_free_env.unwrap_unchecked())(env)
}

/// See [enif_clear_env](http://www.erlang.org/doc/man/erl_nif.html#enif_clear_env) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_clear_env(env: *mut ErlNifEnv)
{
    (DYN_NIF_CALLBACKS.enif_clear_env.unwrap_unchecked())(env)
}

/// See [enif_send](http://www.erlang.org/doc/man/erl_nif.html#enif_send) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_send(env: *mut ErlNifEnv, to_pid: *const ErlNifPid, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_send.unwrap_unchecked())(env, to_pid, msg_env, msg)
}

/// See [enif_make_copy](http://www.erlang.org/doc/man/erl_nif.html#enif_make_copy) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_copy(dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_copy.unwrap_unchecked())(dst_env, src_term)
}

/// See [enif_self](http://www.erlang.org/doc/man/erl_nif.html#enif_self) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_self(caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid)
 -> *mut ErlNifPid{
    (DYN_NIF_CALLBACKS.enif_self.unwrap_unchecked())(caller_env, pid)
}

/// See [enif_get_local_pid](http://www.erlang.org/doc/man/erl_nif.html#enif_get_local_pid) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_local_pid(env: *mut ErlNifEnv, arg2: ERL_NIF_TERM, pid: *mut ErlNifPid)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_local_pid.unwrap_unchecked())(env, arg2, pid)
}

/// See [enif_keep_resource](http://www.erlang.org/doc/man/erl_nif.html#enif_keep_resource) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_keep_resource(obj: *const c_void)
{
    (DYN_NIF_CALLBACKS.enif_keep_resource.unwrap_unchecked())(obj)
}

/// See [enif_make_resource_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_make_resource_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_resource_binary(arg1: *mut ErlNifEnv, obj: *const c_void, data: *const c_void, size: size_t)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_resource_binary.unwrap_unchecked())(arg1, obj, data, size)
}

/// See [enif_get_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int64) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_int64(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_int64.unwrap_unchecked())(arg1, term, ip)
}

/// See [enif_get_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint64) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_uint64(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_uint64.unwrap_unchecked())(arg1, term, ip)
}

/// See [enif_make_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int64) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_int64(arg1: *mut ErlNifEnv, arg2: i64)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_int64.unwrap_unchecked())(arg1, arg2)
}

/// See [enif_make_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint64) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_uint64(arg1: *mut ErlNifEnv, arg2: u64)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_uint64.unwrap_unchecked())(arg1, arg2)
}

/// See [enif_is_exception](http://www.erlang.org/doc/man/erl_nif.html#enif_is_exception) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_exception(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_exception.unwrap_unchecked())(arg1, term)
}

/// See [enif_make_reverse_list](http://www.erlang.org/doc/man/erl_nif.html#enif_make_reverse_list) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_reverse_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, list: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_make_reverse_list.unwrap_unchecked())(arg1, term, list)
}

/// See [enif_is_number](http://www.erlang.org/doc/man/erl_nif.html#enif_is_number) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_number(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_number.unwrap_unchecked())(arg1, term)
}

/// See [enif_dlopen](http://www.erlang.org/doc/man/erl_nif.html#enif_dlopen) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_dlopen(lib: *const c_char, err_handler: Option<unsafe extern "C" fn (*mut c_void, *const c_char)>, err_arg: *mut c_void)
 -> *mut c_void{
    (DYN_NIF_CALLBACKS.enif_dlopen.unwrap_unchecked())(lib, err_handler, err_arg)
}

/// See [enif_dlsym](http://www.erlang.org/doc/man/erl_nif.html#enif_dlsym) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_dlsym(handle: *mut c_void, symbol: *const c_char, err_handler: Option<unsafe extern "C" fn (*mut c_void, *const c_char)>, err_arg: *mut c_void)
 -> *mut c_void{
    (DYN_NIF_CALLBACKS.enif_dlsym.unwrap_unchecked())(handle, symbol, err_handler, err_arg)
}

/// See [enif_consume_timeslice](http://www.erlang.org/doc/man/erl_nif.html#enif_consume_timeslice) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_consume_timeslice(arg1: *mut ErlNifEnv, percent: c_int)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_consume_timeslice.unwrap_unchecked())(arg1, percent)
}

/// See [enif_is_map](http://www.erlang.org/doc/man/erl_nif.html#enif_is_map) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_map(env: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_map.unwrap_unchecked())(env, term)
}

/// See [enif_get_map_size](http://www.erlang.org/doc/man/erl_nif.html#enif_get_map_size) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_map_size(env: *mut ErlNifEnv, term: ERL_NIF_TERM, size: *mut size_t)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_map_size.unwrap_unchecked())(env, term, size)
}

/// See [enif_make_new_map](http://www.erlang.org/doc/man/erl_nif.html#enif_make_new_map) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_new_map(env: *mut ErlNifEnv)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_new_map.unwrap_unchecked())(env)
}

/// See [enif_make_map_put](http://www.erlang.org/doc/man/erl_nif.html#enif_make_map_put) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_map_put(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_make_map_put.unwrap_unchecked())(env, map_in, key, value, map_out)
}

/// See [enif_get_map_value](http://www.erlang.org/doc/man/erl_nif.html#enif_get_map_value) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_map_value(env: *mut ErlNifEnv, map: ERL_NIF_TERM, key: ERL_NIF_TERM, value: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_map_value.unwrap_unchecked())(env, map, key, value)
}

/// See [enif_make_map_update](http://www.erlang.org/doc/man/erl_nif.html#enif_make_map_update) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_map_update(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_make_map_update.unwrap_unchecked())(env, map_in, key, value, map_out)
}

/// See [enif_make_map_remove](http://www.erlang.org/doc/man/erl_nif.html#enif_make_map_remove) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_map_remove(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_make_map_remove.unwrap_unchecked())(env, map_in, key, map_out)
}

/// See [enif_map_iterator_create](http://www.erlang.org/doc/man/erl_nif.html#enif_map_iterator_create) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_map_iterator_create(env: *mut ErlNifEnv, map: ERL_NIF_TERM, iter: *mut ErlNifMapIterator, entry: ErlNifMapIteratorEntry)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_map_iterator_create.unwrap_unchecked())(env, map, iter, entry)
}

/// See [enif_map_iterator_destroy](http://www.erlang.org/doc/man/erl_nif.html#enif_map_iterator_destroy) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_map_iterator_destroy(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator)
{
    (DYN_NIF_CALLBACKS.enif_map_iterator_destroy.unwrap_unchecked())(env, iter)
}

/// See [enif_map_iterator_is_head](http://www.erlang.org/doc/man/erl_nif.html#enif_map_iterator_is_head) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_map_iterator_is_head(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_map_iterator_is_head.unwrap_unchecked())(env, iter)
}

/// See [enif_map_iterator_is_tail](http://www.erlang.org/doc/man/erl_nif.html#enif_map_iterator_is_tail) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_map_iterator_is_tail(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_map_iterator_is_tail.unwrap_unchecked())(env, iter)
}

/// See [enif_map_iterator_next](http://www.erlang.org/doc/man/erl_nif.html#enif_map_iterator_next) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_map_iterator_next(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_map_iterator_next.unwrap_unchecked())(env, iter)
}

/// See [enif_map_iterator_prev](http://www.erlang.org/doc/man/erl_nif.html#enif_map_iterator_prev) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_map_iterator_prev(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_map_iterator_prev.unwrap_unchecked())(env, iter)
}

/// See [enif_map_iterator_get_pair](http://www.erlang.org/doc/man/erl_nif.html#enif_map_iterator_get_pair) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_map_iterator_get_pair(env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator, key: *mut ERL_NIF_TERM, value: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_map_iterator_get_pair.unwrap_unchecked())(env, iter, key, value)
}

/// See [enif_schedule_nif](http://www.erlang.org/doc/man/erl_nif.html#enif_schedule_nif) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_schedule_nif(arg1: *mut ErlNifEnv, arg2: *const c_char, arg3: c_int, arg4: unsafe extern "C" fn(*mut ErlNifEnv, c_int, *const ERL_NIF_TERM) -> ERL_NIF_TERM, arg5: c_int, arg6: *const ERL_NIF_TERM)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_schedule_nif.unwrap_unchecked())(arg1, arg2, arg3, arg4, arg5, arg6)
}

/// See [enif_has_pending_exception](http://www.erlang.org/doc/man/erl_nif.html#enif_has_pending_exception) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_has_pending_exception(env: *mut ErlNifEnv, reason: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_has_pending_exception.unwrap_unchecked())(env, reason)
}

/// See [enif_raise_exception](http://www.erlang.org/doc/man/erl_nif.html#enif_raise_exception) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_raise_exception(env: *mut ErlNifEnv, reason: ERL_NIF_TERM)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_raise_exception.unwrap_unchecked())(env, reason)
}

/// See [enif_getenv](http://www.erlang.org/doc/man/erl_nif.html#enif_getenv) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_getenv(key: *const c_char, value: *mut c_char, value_size: *mut size_t)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_getenv.unwrap_unchecked())(key, value, value_size)
}

/// See [enif_monotonic_time](http://www.erlang.org/doc/man/erl_nif.html#enif_monotonic_time) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_monotonic_time(arg1: ErlNifTimeUnit)
 -> ErlNifTime{
    (DYN_NIF_CALLBACKS.enif_monotonic_time.unwrap_unchecked())(arg1)
}

/// See [enif_time_offset](http://www.erlang.org/doc/man/erl_nif.html#enif_time_offset) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_time_offset(arg1: ErlNifTimeUnit)
 -> ErlNifTime{
    (DYN_NIF_CALLBACKS.enif_time_offset.unwrap_unchecked())(arg1)
}

/// See [enif_convert_time_unit](http://www.erlang.org/doc/man/erl_nif.html#enif_convert_time_unit) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_convert_time_unit(arg1: ErlNifTime, arg2: ErlNifTimeUnit, arg3: ErlNifTimeUnit)
 -> ErlNifTime{
    (DYN_NIF_CALLBACKS.enif_convert_time_unit.unwrap_unchecked())(arg1, arg2, arg3)
}

/// See [enif_now_time](http://www.erlang.org/doc/man/erl_nif.html#enif_now_time) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_now_time(env: *mut ErlNifEnv)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_now_time.unwrap_unchecked())(env)
}

/// See [enif_cpu_time](http://www.erlang.org/doc/man/erl_nif.html#enif_cpu_time) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_cpu_time(env: *mut ErlNifEnv)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_cpu_time.unwrap_unchecked())(env)
}

/// See [enif_make_unique_integer](http://www.erlang.org/doc/man/erl_nif.html#enif_make_unique_integer) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_unique_integer(env: *mut ErlNifEnv, properties: ErlNifUniqueInteger)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_unique_integer.unwrap_unchecked())(env, properties)
}

/// See [enif_is_current_process_alive](http://www.erlang.org/doc/man/erl_nif.html#enif_is_current_process_alive) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_current_process_alive(env: *mut ErlNifEnv)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_current_process_alive.unwrap_unchecked())(env)
}

/// See [enif_is_process_alive](http://www.erlang.org/doc/man/erl_nif.html#enif_is_process_alive) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_process_alive(env: *mut ErlNifEnv, pid: *const ErlNifPid)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_process_alive.unwrap_unchecked())(env, pid)
}

/// See [enif_is_port_alive](http://www.erlang.org/doc/man/erl_nif.html#enif_is_port_alive) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_port_alive(env: *mut ErlNifEnv, port_id: *mut ErlNifPort)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_port_alive.unwrap_unchecked())(env, port_id)
}

/// See [enif_get_local_port](http://www.erlang.org/doc/man/erl_nif.html#enif_get_local_port) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_get_local_port(env: *mut ErlNifEnv, arg2: ERL_NIF_TERM, port_id: *mut ErlNifPort)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_get_local_port.unwrap_unchecked())(env, arg2, port_id)
}

/// See [enif_term_to_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_term_to_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_term_to_binary(env: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_term_to_binary.unwrap_unchecked())(env, term, bin)
}

/// See [enif_binary_to_term](http://www.erlang.org/doc/man/erl_nif.html#enif_binary_to_term) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_binary_to_term(env: *mut ErlNifEnv, data: *const c_uchar, sz: size_t, term: *mut ERL_NIF_TERM, opts: c_uint)
 -> size_t{
    (DYN_NIF_CALLBACKS.enif_binary_to_term.unwrap_unchecked())(env, data, sz, term, opts)
}

/// See [enif_port_command](http://www.erlang.org/doc/man/erl_nif.html#enif_port_command) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_port_command(env: *mut ErlNifEnv, to_port: *const ErlNifPort, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_port_command.unwrap_unchecked())(env, to_port, msg_env, msg)
}

/// See [enif_thread_type](http://www.erlang.org/doc/man/erl_nif.html#enif_thread_type) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_thread_type()
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_thread_type.unwrap_unchecked())()
}

#[macro_export] macro_rules! enif_snprintf {
    ( $( $arg:expr ),* ) => { $crate::sys::get_enif_snprintf()($($arg),*) };
    ( $( $arg:expr ),+, ) => { enif_snprintf!($($arg),*) };
}

pub use enif_snprintf;

pub unsafe fn get_enif_snprintf() -> extern "C" fn (buffer: *mut c_char, size: size_t, format: *const c_char, ...) -> c_int {
    DYN_NIF_CALLBACKS.enif_snprintf.unwrap_unchecked()
}

/// See [enif_select](http://www.erlang.org/doc/man/erl_nif.html#enif_select) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_select(env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *mut c_void, pid: *const ErlNifPid, ref_: ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_select.unwrap_unchecked())(env, e, flags, obj, pid, ref_)
}

/// See [enif_open_resource_type_x](http://www.erlang.org/doc/man/erl_nif.html#enif_open_resource_type_x) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_open_resource_type_x(arg1: *mut ErlNifEnv, name_str: *const c_char, arg3: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags)
 -> *const ErlNifResourceType{
    (DYN_NIF_CALLBACKS.enif_open_resource_type_x.unwrap_unchecked())(arg1, name_str, arg3, flags, tried)
}

/// See [enif_monitor_process](http://www.erlang.org/doc/man/erl_nif.html#enif_monitor_process) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_monitor_process(arg1: *mut ErlNifEnv, obj: *const c_void, arg3: *const ErlNifPid, monitor: *mut ErlNifMonitor)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_monitor_process.unwrap_unchecked())(arg1, obj, arg3, monitor)
}

/// See [enif_demonitor_process](http://www.erlang.org/doc/man/erl_nif.html#enif_demonitor_process) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_demonitor_process(arg1: *mut ErlNifEnv, obj: *const c_void, monitor: *const ErlNifMonitor)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_demonitor_process.unwrap_unchecked())(arg1, obj, monitor)
}

/// See [enif_compare_monitors](http://www.erlang.org/doc/man/erl_nif.html#enif_compare_monitors) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_compare_monitors(arg1: *const ErlNifMonitor, arg2: *const ErlNifMonitor)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_compare_monitors.unwrap_unchecked())(arg1, arg2)
}

/// See [enif_hash](http://www.erlang.org/doc/man/erl_nif.html#enif_hash) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_hash(type_: ErlNifHash, term: ERL_NIF_TERM, salt: u64)
 -> u64{
    (DYN_NIF_CALLBACKS.enif_hash.unwrap_unchecked())(type_, term, salt)
}

/// See [enif_whereis_pid](http://www.erlang.org/doc/man/erl_nif.html#enif_whereis_pid) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_whereis_pid(env: *mut ErlNifEnv, name: ERL_NIF_TERM, pid: *mut ErlNifPid)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_whereis_pid.unwrap_unchecked())(env, name, pid)
}

/// See [enif_whereis_port](http://www.erlang.org/doc/man/erl_nif.html#enif_whereis_port) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_whereis_port(env: *mut ErlNifEnv, name: ERL_NIF_TERM, port: *mut ErlNifPort)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_whereis_port.unwrap_unchecked())(env, name, port)
}

/// See [enif_ioq_create](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_create) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_create(opts: ErlNifIOQueueOpts)
 -> *mut ErlNifIOQueue{
    (DYN_NIF_CALLBACKS.enif_ioq_create.unwrap_unchecked())(opts)
}

/// See [enif_ioq_destroy](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_destroy) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_destroy(q: *mut ErlNifIOQueue)
{
    (DYN_NIF_CALLBACKS.enif_ioq_destroy.unwrap_unchecked())(q)
}

/// See [enif_ioq_enq_binary](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_enq_binary) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_enq_binary(q: *mut ErlNifIOQueue, bin: *mut ErlNifBinary, skip: size_t)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_ioq_enq_binary.unwrap_unchecked())(q, bin, skip)
}

/// See [enif_ioq_enqv](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_enqv) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_enqv(q: *mut ErlNifIOQueue, iov: *mut ErlNifIOVec, skip: size_t)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_ioq_enqv.unwrap_unchecked())(q, iov, skip)
}

/// See [enif_ioq_size](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_size) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_size(q: *mut ErlNifIOQueue)
 -> size_t{
    (DYN_NIF_CALLBACKS.enif_ioq_size.unwrap_unchecked())(q)
}

/// See [enif_ioq_deq](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_deq) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_deq(q: *mut ErlNifIOQueue, count: size_t, size: *mut size_t)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_ioq_deq.unwrap_unchecked())(q, count, size)
}

/// See [enif_ioq_peek](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_peek) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_peek(q: *mut ErlNifIOQueue, iovlen: *mut c_int)
 -> *mut SysIOVec{
    (DYN_NIF_CALLBACKS.enif_ioq_peek.unwrap_unchecked())(q, iovlen)
}

/// See [enif_inspect_iovec](http://www.erlang.org/doc/man/erl_nif.html#enif_inspect_iovec) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_inspect_iovec(env: *mut ErlNifEnv, max_length: size_t, iovec_term: ERL_NIF_TERM, tail: *mut ERL_NIF_TERM, iovec: *mut *mut ErlNifIOVec)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_inspect_iovec.unwrap_unchecked())(env, max_length, iovec_term, tail, iovec)
}

/// See [enif_free_iovec](http://www.erlang.org/doc/man/erl_nif.html#enif_free_iovec) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_free_iovec(iov: *mut ErlNifIOVec)
{
    (DYN_NIF_CALLBACKS.enif_free_iovec.unwrap_unchecked())(iov)
}

/// See [enif_ioq_peek_head](http://www.erlang.org/doc/man/erl_nif.html#enif_ioq_peek_head) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_ioq_peek_head(env: *mut ErlNifEnv, q: *mut ErlNifIOQueue, size: *mut size_t, head: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_ioq_peek_head.unwrap_unchecked())(env, q, size, head)
}

/// See [enif_make_map_from_arrays](http://www.erlang.org/doc/man/erl_nif.html#enif_make_map_from_arrays) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_map_from_arrays(env: *mut ErlNifEnv, keys: *const ERL_NIF_TERM, values: *const ERL_NIF_TERM, cnt: size_t, map_out: *mut ERL_NIF_TERM)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_make_map_from_arrays.unwrap_unchecked())(env, keys, values, cnt, map_out)
}

/// See [enif_select_x](http://www.erlang.org/doc/man/erl_nif.html#enif_select_x) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_select_x(env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *mut c_void, pid: *const ErlNifPid, msg: ERL_NIF_TERM, msg_env: *mut ErlNifEnv)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_select_x.unwrap_unchecked())(env, e, flags, obj, pid, msg, msg_env)
}

/// See [enif_make_monitor_term](http://www.erlang.org/doc/man/erl_nif.html#enif_make_monitor_term) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_make_monitor_term(env: *mut ErlNifEnv, arg2: *const ErlNifMonitor)
 -> ERL_NIF_TERM{
    (DYN_NIF_CALLBACKS.enif_make_monitor_term.unwrap_unchecked())(env, arg2)
}

/// See [enif_set_pid_undefined](http://www.erlang.org/doc/man/erl_nif.html#enif_set_pid_undefined) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_set_pid_undefined(pid: *mut ErlNifPid)
{
    (DYN_NIF_CALLBACKS.enif_set_pid_undefined.unwrap_unchecked())(pid)
}

/// See [enif_is_pid_undefined](http://www.erlang.org/doc/man/erl_nif.html#enif_is_pid_undefined) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_is_pid_undefined(pid: *const ErlNifPid)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_is_pid_undefined.unwrap_unchecked())(pid)
}

/// See [enif_term_type](http://www.erlang.org/doc/man/erl_nif.html#enif_term_type) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_term_type(env: *mut ErlNifEnv, term: ERL_NIF_TERM)
 -> ErlNifTermType{
    (DYN_NIF_CALLBACKS.enif_term_type.unwrap_unchecked())(env, term)
}

/// See [enif_init_resource_type](http://www.erlang.org/doc/man/erl_nif.html#enif_init_resource_type) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_init_resource_type(arg1: *mut ErlNifEnv, name_str: *const c_char, arg3: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags)
 -> *const ErlNifResourceType{
    (DYN_NIF_CALLBACKS.enif_init_resource_type.unwrap_unchecked())(arg1, name_str, arg3, flags, tried)
}

/// See [enif_dynamic_resource_call](http://www.erlang.org/doc/man/erl_nif.html#enif_dynamic_resource_call) in the Erlang docs.
#[inline]
pub unsafe extern "C" fn enif_dynamic_resource_call(arg1: *mut ErlNifEnv, mod_: ERL_NIF_TERM, name: ERL_NIF_TERM, rsrc: ERL_NIF_TERM, call_data: *mut c_void)
 -> c_int{
    (DYN_NIF_CALLBACKS.enif_dynamic_resource_call.unwrap_unchecked())(arg1, mod_, name, rsrc, call_data)
}

impl DynNifCallbacks {
    fn write_symbols<T: DynNifFiller>(&mut self, filler: T) {
        filler.write(&mut self.enif_priv_data, "enif_priv_data ");
        filler.write(&mut self.enif_alloc, "enif_alloc ");
        filler.write(&mut self.enif_free, "enif_free ");
        filler.write(&mut self.enif_is_atom, "enif_is_atom ");
        filler.write(&mut self.enif_is_binary, "enif_is_binary ");
        filler.write(&mut self.enif_is_ref, "enif_is_ref ");
        filler.write(&mut self.enif_inspect_binary, "enif_inspect_binary ");
        filler.write(&mut self.enif_alloc_binary, "enif_alloc_binary ");
        filler.write(&mut self.enif_realloc_binary, "enif_realloc_binary ");
        filler.write(&mut self.enif_release_binary, "enif_release_binary ");
        filler.write(&mut self.enif_get_int, "enif_get_int ");
        filler.write(&mut self.enif_get_ulong, "enif_get_ulong ");
        filler.write(&mut self.enif_get_double, "enif_get_double ");
        filler.write(&mut self.enif_get_list_cell, "enif_get_list_cell ");
        filler.write(&mut self.enif_get_tuple, "enif_get_tuple ");
        filler.write(&mut self.enif_is_identical, "enif_is_identical ");
        filler.write(&mut self.enif_compare, "enif_compare ");
        filler.write(&mut self.enif_make_binary, "enif_make_binary ");
        filler.write(&mut self.enif_make_badarg, "enif_make_badarg ");
        filler.write(&mut self.enif_make_int, "enif_make_int ");
        filler.write(&mut self.enif_make_ulong, "enif_make_ulong ");
        filler.write(&mut self.enif_make_double, "enif_make_double ");
        filler.write(&mut self.enif_make_atom, "enif_make_atom ");
        filler.write(&mut self.enif_make_existing_atom, "enif_make_existing_atom ");
        filler.write(&mut self.enif_make_tuple, "enif_make_tuple ");
        filler.write(&mut self.enif_make_list, "enif_make_list ");
        filler.write(&mut self.enif_make_list_cell, "enif_make_list_cell ");
        filler.write(&mut self.enif_make_string, "enif_make_string ");
        filler.write(&mut self.enif_make_ref, "enif_make_ref ");
        filler.write(&mut self.enif_realloc, "enif_realloc ");
        filler.write(&mut self.enif_system_info, "enif_system_info ");
        filler.write(&mut self.enif_fprintf, "enif_fprintf ");
        filler.write(&mut self.enif_inspect_iolist_as_binary, "enif_inspect_iolist_as_binary ");
        filler.write(&mut self.enif_make_sub_binary, "enif_make_sub_binary ");
        filler.write(&mut self.enif_get_string, "enif_get_string ");
        filler.write(&mut self.enif_get_atom, "enif_get_atom ");
        filler.write(&mut self.enif_is_fun, "enif_is_fun ");
        filler.write(&mut self.enif_is_pid, "enif_is_pid ");
        filler.write(&mut self.enif_is_port, "enif_is_port ");
        filler.write(&mut self.enif_get_uint, "enif_get_uint ");
        filler.write(&mut self.enif_get_long, "enif_get_long ");
        filler.write(&mut self.enif_make_uint, "enif_make_uint ");
        filler.write(&mut self.enif_make_long, "enif_make_long ");
        filler.write(&mut self.enif_make_tuple_from_array, "enif_make_tuple_from_array ");
        filler.write(&mut self.enif_make_list_from_array, "enif_make_list_from_array ");
        filler.write(&mut self.enif_is_empty_list, "enif_is_empty_list ");
        filler.write(&mut self.enif_open_resource_type, "enif_open_resource_type ");
        filler.write(&mut self.enif_alloc_resource, "enif_alloc_resource ");
        filler.write(&mut self.enif_release_resource, "enif_release_resource ");
        filler.write(&mut self.enif_make_resource, "enif_make_resource ");
        filler.write(&mut self.enif_get_resource, "enif_get_resource ");
        filler.write(&mut self.enif_sizeof_resource, "enif_sizeof_resource ");
        filler.write(&mut self.enif_make_new_binary, "enif_make_new_binary ");
        filler.write(&mut self.enif_is_list, "enif_is_list ");
        filler.write(&mut self.enif_is_tuple, "enif_is_tuple ");
        filler.write(&mut self.enif_get_atom_length, "enif_get_atom_length ");
        filler.write(&mut self.enif_get_list_length, "enif_get_list_length ");
        filler.write(&mut self.enif_make_atom_len, "enif_make_atom_len ");
        filler.write(&mut self.enif_make_existing_atom_len, "enif_make_existing_atom_len ");
        filler.write(&mut self.enif_make_string_len, "enif_make_string_len ");
        filler.write(&mut self.enif_alloc_env, "enif_alloc_env ");
        filler.write(&mut self.enif_free_env, "enif_free_env ");
        filler.write(&mut self.enif_clear_env, "enif_clear_env ");
        filler.write(&mut self.enif_send, "enif_send ");
        filler.write(&mut self.enif_make_copy, "enif_make_copy ");
        filler.write(&mut self.enif_self, "enif_self ");
        filler.write(&mut self.enif_get_local_pid, "enif_get_local_pid ");
        filler.write(&mut self.enif_keep_resource, "enif_keep_resource ");
        filler.write(&mut self.enif_make_resource_binary, "enif_make_resource_binary ");
        filler.write(&mut self.enif_get_int64, "enif_get_int64 ");
        filler.write(&mut self.enif_get_uint64, "enif_get_uint64 ");
        filler.write(&mut self.enif_make_int64, "enif_make_int64 ");
        filler.write(&mut self.enif_make_uint64, "enif_make_uint64 ");
        filler.write(&mut self.enif_is_exception, "enif_is_exception ");
        filler.write(&mut self.enif_make_reverse_list, "enif_make_reverse_list ");
        filler.write(&mut self.enif_is_number, "enif_is_number ");
        filler.write(&mut self.enif_dlopen, "enif_dlopen ");
        filler.write(&mut self.enif_dlsym, "enif_dlsym ");
        filler.write(&mut self.enif_consume_timeslice, "enif_consume_timeslice ");
        filler.write(&mut self.enif_is_map, "enif_is_map ");
        filler.write(&mut self.enif_get_map_size, "enif_get_map_size ");
        filler.write(&mut self.enif_make_new_map, "enif_make_new_map ");
        filler.write(&mut self.enif_make_map_put, "enif_make_map_put ");
        filler.write(&mut self.enif_get_map_value, "enif_get_map_value ");
        filler.write(&mut self.enif_make_map_update, "enif_make_map_update ");
        filler.write(&mut self.enif_make_map_remove, "enif_make_map_remove ");
        filler.write(&mut self.enif_map_iterator_create, "enif_map_iterator_create ");
        filler.write(&mut self.enif_map_iterator_destroy, "enif_map_iterator_destroy ");
        filler.write(&mut self.enif_map_iterator_is_head, "enif_map_iterator_is_head ");
        filler.write(&mut self.enif_map_iterator_is_tail, "enif_map_iterator_is_tail ");
        filler.write(&mut self.enif_map_iterator_next, "enif_map_iterator_next ");
        filler.write(&mut self.enif_map_iterator_prev, "enif_map_iterator_prev ");
        filler.write(&mut self.enif_map_iterator_get_pair, "enif_map_iterator_get_pair ");
        filler.write(&mut self.enif_schedule_nif, "enif_schedule_nif ");
        filler.write(&mut self.enif_has_pending_exception, "enif_has_pending_exception ");
        filler.write(&mut self.enif_raise_exception, "enif_raise_exception ");
        filler.write(&mut self.enif_getenv, "enif_getenv ");
        filler.write(&mut self.enif_monotonic_time, "enif_monotonic_time ");
        filler.write(&mut self.enif_time_offset, "enif_time_offset ");
        filler.write(&mut self.enif_convert_time_unit, "enif_convert_time_unit ");
        filler.write(&mut self.enif_now_time, "enif_now_time ");
        filler.write(&mut self.enif_cpu_time, "enif_cpu_time ");
        filler.write(&mut self.enif_make_unique_integer, "enif_make_unique_integer ");
        filler.write(&mut self.enif_is_current_process_alive, "enif_is_current_process_alive ");
        filler.write(&mut self.enif_is_process_alive, "enif_is_process_alive ");
        filler.write(&mut self.enif_is_port_alive, "enif_is_port_alive ");
        filler.write(&mut self.enif_get_local_port, "enif_get_local_port ");
        filler.write(&mut self.enif_term_to_binary, "enif_term_to_binary ");
        filler.write(&mut self.enif_binary_to_term, "enif_binary_to_term ");
        filler.write(&mut self.enif_port_command, "enif_port_command ");
        filler.write(&mut self.enif_thread_type, "enif_thread_type ");
        filler.write(&mut self.enif_snprintf, "enif_snprintf ");
        filler.write(&mut self.enif_select, "enif_select ");
        filler.write(&mut self.enif_open_resource_type_x, "enif_open_resource_type_x ");
        filler.write(&mut self.enif_monitor_process, "enif_monitor_process ");
        filler.write(&mut self.enif_demonitor_process, "enif_demonitor_process ");
        filler.write(&mut self.enif_compare_monitors, "enif_compare_monitors ");
        filler.write(&mut self.enif_hash, "enif_hash ");
        filler.write(&mut self.enif_whereis_pid, "enif_whereis_pid ");
        filler.write(&mut self.enif_whereis_port, "enif_whereis_port ");
        filler.write(&mut self.enif_ioq_create, "enif_ioq_create ");
        filler.write(&mut self.enif_ioq_destroy, "enif_ioq_destroy ");
        filler.write(&mut self.enif_ioq_enq_binary, "enif_ioq_enq_binary ");
        filler.write(&mut self.enif_ioq_enqv, "enif_ioq_enqv ");
        filler.write(&mut self.enif_ioq_size, "enif_ioq_size ");
        filler.write(&mut self.enif_ioq_deq, "enif_ioq_deq ");
        filler.write(&mut self.enif_ioq_peek, "enif_ioq_peek ");
        filler.write(&mut self.enif_inspect_iovec, "enif_inspect_iovec ");
        filler.write(&mut self.enif_free_iovec, "enif_free_iovec ");
        filler.write(&mut self.enif_ioq_peek_head, "enif_ioq_peek_head ");
        filler.write(&mut self.enif_make_map_from_arrays, "enif_make_map_from_arrays ");
        filler.write(&mut self.enif_select_x, "enif_select_x ");
        filler.write(&mut self.enif_make_monitor_term, "enif_make_monitor_term ");
        filler.write(&mut self.enif_set_pid_undefined, "enif_set_pid_undefined ");
        filler.write(&mut self.enif_is_pid_undefined, "enif_is_pid_undefined ");
        filler.write(&mut self.enif_term_type, "enif_term_type ");
        filler.write(&mut self.enif_init_resource_type, "enif_init_resource_type ");
        filler.write(&mut self.enif_dynamic_resource_call, "enif_dynamic_resource_call ");
    }
}
