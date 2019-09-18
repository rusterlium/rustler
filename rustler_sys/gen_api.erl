%% gen_api

%% api_list
%%
%% This is the API list that corresponds to the list found in erl_nif_api_funcs.h
%% Initial conversion provided by https://github.com/lavrin/erlang-rust-nif/issues/2
%%
%% Functions that are not supported for Rust must still be listed so that
%% the Windows callback struct keeps proper integrity.  Such functions
%% must begin with "dummy."
%%

-type api_opt() ::
    exception              |  % enif_xxx_exception() functions
    getenv                 |  % enif_getenv() functions
    time                   |  % new timer API
    {ulongsize, integer()} |  % number of bytes in a C ulong
    dirty_schedulers       |  % enif_is_on_dirty_scheduler().  Only for 2.7-2.10
    dirty_scheduler_opt    |  % dirty scheduler nifentry option flag.  For >=2.7
    nif_2_11               .  % general 2.11 API additions


version_opts("2.7")  -> [{major,2}, {minor,7} ];                           % erlang 17.3
version_opts("2.8")  -> [{major,2}, {minor,8},  exception];                % erlang 18.0
version_opts("2.9")  -> [{major,2}, {minor,9},  exception, getenv];        % erlang 18.2
version_opts("2.10") -> [{major,2}, {minor,10}, exception, getenv, time];  % erlang 18.3
version_opts("2.11") -> [{major,2}, {minor,11}, exception, getenv, time,   % erlang 19.0
                        dirty_scheduler_opt, nif_2_11];
version_opts("2.12") -> [{major,2}, {minor,12}, exception, getenv, time,   % erlang 20.0
                        dirty_scheduler_opt, nif_2_11, nif_2_12];
version_opts("2.13") -> [{major,2}, {minor,13}, exception, getenv, time,   % erlang 20.1
                        dirty_scheduler_opt, nif_2_11, nif_2_12, nif_2_13];
version_opts("2.14") -> [{major,2}, {minor,14}, exception, getenv, time,   % erlang 21.0
                        dirty_scheduler_opt, nif_2_11, nif_2_12, nif_2_13,
                        nif_2_14];
version_opts("2.15") -> [{major,2}, {minor,15}, exception, getenv, time,   % erlang 22.0
                        dirty_scheduler_opt, nif_2_11, nif_2_12, nif_2_13,
                        nif_2_14, nif_2_15];
version_opts(Ver) ->
    io:format(
        "This OTP release uses the unsupported Erlang NIF version ~p.\n\n"
        "Please report at https://github.com/rustlerium/rustler.\n",
        [Ver]
    ),
    halt(1).

ulong_opts("4") -> [{ulongsize, 4}];
ulong_opts("8") -> [{ulongsize, 8}].


dirty_scheduler_opts("2.7") -> dirty_scheduler_opts();
dirty_scheduler_opts("2.8") -> dirty_scheduler_opts();
dirty_scheduler_opts("2.9") -> dirty_scheduler_opts();
dirty_scheduler_opts("2.10") -> dirty_scheduler_opts();
dirty_scheduler_opts(_) -> []. % dirty schedulers non-optional in 2.11

dirty_scheduler_opts() ->
    case catch erlang:system_info(dirty_cpu_schedulers) of
                                 _X when is_integer(_X) -> [dirty_schedulers, dirty_scheduler_opt];
                                 _ -> []
                             end.


-spec api_list([api_opt()]) -> [term()].
api_list(Opts) -> [
    {"*mut c_void", "enif_priv_data", "arg1: *mut ErlNifEnv"},
    {"*mut c_void", "enif_alloc", "size: size_t"},
    {"", "enif_free", "ptr: *mut c_void"},
    {"c_int", "enif_is_atom", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_is_binary", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_is_ref", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_inspect_binary", "arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, bin: *mut ErlNifBinary"},
    {"c_int", "enif_alloc_binary", "size: size_t, bin: *mut ErlNifBinary"},
    {"c_int", "enif_realloc_binary", "bin: *mut ErlNifBinary, size: size_t"},
    {"", "enif_release_binary", "bin: *mut ErlNifBinary"},
    {"c_int", "enif_get_int", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_int"},
    {"c_int", "enif_get_ulong", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulong"},
    {"c_int", "enif_get_double", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, dp: *mut c_double"},
    {"c_int", "enif_get_list_cell", "env: *mut ErlNifEnv, term: ERL_NIF_TERM, head: *mut ERL_NIF_TERM, tail: *mut ERL_NIF_TERM"},
    {"c_int", "enif_get_tuple", "env: *mut ErlNifEnv, tpl: ERL_NIF_TERM, arity: *mut c_int, array: *mut *const ERL_NIF_TERM"},
    {"c_int", "enif_is_identical", "lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM"},
    {"c_int", "enif_compare", "lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM"},
    {"ERL_NIF_TERM", "enif_make_binary", "env: *mut ErlNifEnv, bin: *mut ErlNifBinary"},
    {"ERL_NIF_TERM", "enif_make_badarg", "env: *mut ErlNifEnv"},
    {"ERL_NIF_TERM", "enif_make_int", "env: *mut ErlNifEnv, i: c_int"},
    {"ERL_NIF_TERM", "enif_make_ulong", "env: *mut ErlNifEnv, i: c_ulong"},
    {"ERL_NIF_TERM", "enif_make_double", "env: *mut ErlNifEnv, d: c_double"},
    {"ERL_NIF_TERM", "enif_make_atom", "env: *mut ErlNifEnv, name: *const c_uchar"},
    {"c_int", "enif_make_existing_atom", "env: *mut ErlNifEnv, name: *const c_uchar, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding"},

    {"ERL_NIF_TERM", "enif_make_tuple", "env: *mut ErlNifEnv, cnt: c_uint, ..."},
    {"ERL_NIF_TERM", "enif_make_list", "env: *mut ErlNifEnv, cnt: c_uint, ..."},

    {"ERL_NIF_TERM", "enif_make_list_cell", "env: *mut ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM"},
    {"ERL_NIF_TERM", "enif_make_string", "env: *mut ErlNifEnv, string: *const c_uchar, arg1: ErlNifCharEncoding"},
    {"ERL_NIF_TERM", "enif_make_ref", "env: *mut ErlNifEnv"},

    %% Skip threading API for now (perhaps forever)
    %% If anybody has a situation where they want to use this API instead of the very fine
    %% Rust API, please tell me.
    %%      {"*mut ErlNifMutex", "enif_mutex_create", "name: *mut c_uchar"},
    %%      {"", "enif_mutex_destroy", "mtx: *mut ErlNifMutex"},
    %%      {"c_int", "enif_mutex_trylock", "mtx: *mut ErlNifMutex"},
    %%      {"", "enif_mutex_lock", "mtx: *mut ErlNifMutex"},
    %%      {"", "enif_mutex_unlock", "mtx: *mut ErlNifMutex"},
    %%      {"*mut ErlNifCond", "enif_cond_create", "name: *mut c_uchar"},
    %%      {"", "enif_cond_destroy", "cnd: *mut ErlNifCond"},
    %%      {"", "enif_cond_signal", "cnd: *mut ErlNifCond"},
    %%      {"", "enif_cond_broadcast", "cnd: *mut ErlNifCond"},
    %%      {"", "enif_cond_wait", "cnd: *mut ErlNifCond, mtx: *mut ErlNifMutex"},
    %%      {"*mut ErlNifRWLock", "enif_rwlock_create", "name: *mut c_uchar"},
    %%      {"", "enif_rwlock_destroy", "rwlck: *mut ErlNifRWLock"},
    %%      {"c_int", "enif_rwlock_tryrlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_rlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_runlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"c_int", "enif_rwlock_tryrwlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_rwlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_rwunlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"c_int", "enif_tsd_key_create", "name: *mut c_uchar, key: *mut ErlNifTSDKey"},
    %%      {"", "enif_tsd_key_destroy", "key: ErlNifTSDKey"},
    %%      {"", "enif_tsd_set", "key: ErlNifTSDKey, data: *mut c_void"},
    %%      {"*mut c_void", "enif_tsd_get", "key: ErlNifTSDKey"},
    %%      {"*mut ErlNifThreadOpts", "enif_thread_opts_create", "name: *mut c_uchar"},
    %%      {"", "enif_thread_opts_destroy", "opts: *mut ErlNifThreadOpts"},
    %%      {"c_int", "enif_thread_create", "name: *mut c_uchar, tid: *mut ErlNifTid, func: Option<unsafe extern \"C\" fn (arg1: *mut c_void) -> *mut c_void>, args: *mut c_void, opts: *mut ErlNifThreadOpts"},
    %%      {"ErlNifTid", "enif_thread_self", ""},
    %%      {"c_int", "enif_equal_tids", "tid1: ErlNifTid, tid2: ErlNifTid"},
    %%      {"", "enif_thread_exit", "resp: *mut c_void"},
    %%      {"c_int", "enif_thread_join", "arg1: ErlNifTid, respp: *mut *mut c_void"},
    {"", "dummy_enif_mutex_create", ""},
    {"", "dummy_enif_mutex_destroy", ""},
    {"", "dummy_enif_mutex_trylock", ""},
    {"", "dummy_enif_mutex_lock", ""},
    {"", "dummy_enif_mutex_unlock", ""},
    {"", "dummy_enif_cond_create", ""},
    {"", "dummy_enif_cond_destroy", ""},
    {"", "dummy_enif_cond_signal", ""},
    {"", "dummy_enif_cond_broadcast", ""},
    {"", "dummy_enif_cond_wait", ""},
    {"", "dummy_enif_rwlock_create", ""},
    {"", "dummy_enif_rwlock_destroy", ""},
    {"", "dummy_enif_rwlock_tryrlock", ""},
    {"", "dummy_enif_rwlock_rlock", ""},
    {"", "dummy_enif_rwlock_runlock", ""},
    {"", "dummy_enif_rwlock_tryrwlock", ""},
    {"", "dummy_enif_rwlock_rwlock", ""},
    {"", "dummy_enif_rwlock_rwunlock", ""},
    {"", "dummy_enif_tsd_key_create", ""},
    {"", "dummy_enif_tsd_key_destroy", ""},
    {"", "dummy_enif_tsd_set", ""},
    {"", "dummy_enif_tsd_get", ""},
    {"", "dummy_enif_thread_opts_create", ""},
    {"", "dummy_enif_thread_opts_destroy", ""},
    {"", "dummy_enif_thread_create", ""},
    {"", "dummy_enif_thread_self", ""},
    {"", "dummy_enif_equal_tids", ""},
    {"", "dummy_enif_thread_exit", ""},
    {"", "dummy_enif_thread_join", ""},

    {"*mut c_void", "enif_realloc", "ptr: *mut c_void, size: size_t"},
    {"", "enif_system_info", "sip: *mut ErlNifSysInfo, si_size: size_t"},

    {"c_int", "enif_fprintf", "filep: *mut c_void, format: *const c_uchar, ..."},

    {"c_int", "enif_inspect_iolist_as_binary", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary"},
    {"ERL_NIF_TERM", "enif_make_sub_binary", "arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, pos: size_t, size: size_t"},
    {"c_int", "enif_get_string", "arg1: *mut ErlNifEnv, list: ERL_NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding"},
    {"c_int", "enif_get_atom", "arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding"},
    {"c_int", "enif_is_fun", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_is_pid", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_is_port", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_get_uint", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_uint"},
    {"c_int", "enif_get_long", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_long"},
    {"ERL_NIF_TERM", "enif_make_uint", "arg1: *mut ErlNifEnv, i: c_uint"},
    {"ERL_NIF_TERM", "enif_make_long", "arg1: *mut ErlNifEnv, i: c_long"},
    {"ERL_NIF_TERM", "enif_make_tuple_from_array", "arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint"},
    {"ERL_NIF_TERM", "enif_make_list_from_array", "arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint"},
    {"c_int", "enif_is_empty_list", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"*const ErlNifResourceType", "enif_open_resource_type", "arg1: *mut ErlNifEnv, module_str: *const c_uchar, name_str: *const c_uchar, dtor: Option<unsafe extern \"C\" fn (arg1: *mut ErlNifEnv, arg2: *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags"},
    {"*mut c_void", "enif_alloc_resource", "_type: *const ErlNifResourceType, size: size_t"},
    {"", "enif_release_resource", "obj: *const c_void"},
    {"ERL_NIF_TERM", "enif_make_resource", "arg1: *mut ErlNifEnv, obj: *const c_void"},
    {"c_int", "enif_get_resource", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, _type: *const ErlNifResourceType, objp: *mut *const c_void"},
    {"size_t", "enif_sizeof_resource", "obj: *const c_void"},
    {"*mut c_uchar", "enif_make_new_binary", "arg1: *mut ErlNifEnv, size: size_t, termp: *mut ERL_NIF_TERM"},
    {"c_int", "enif_is_list", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_is_tuple", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_get_atom_length", "arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, len: *mut c_uint, arg2: ErlNifCharEncoding"},
    {"c_int", "enif_get_list_length", "env: *mut ErlNifEnv, term: ERL_NIF_TERM, len: *mut c_uint"},
    {"ERL_NIF_TERM", "enif_make_atom_len", "env: *mut ErlNifEnv, name: *const c_uchar, len: size_t"},
    {"c_int", "enif_make_existing_atom_len", "env: *mut ErlNifEnv, name: *const c_uchar, len: size_t, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding"},
    {"ERL_NIF_TERM", "enif_make_string_len", "env: *mut ErlNifEnv, string: *const c_uchar, len: size_t, arg1: ErlNifCharEncoding"},
    {"*mut ErlNifEnv", "enif_alloc_env", ""},
    {"", "enif_free_env", "env: *mut ErlNifEnv"},
    {"", "enif_clear_env", "env: *mut ErlNifEnv"},
    {"c_int", "enif_send", "env: *mut ErlNifEnv, to_pid: *const ErlNifPid, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM"},
    {"ERL_NIF_TERM", "enif_make_copy", "dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM"},
    {"*mut ErlNifPid", "enif_self", "caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid"},
    {"c_int", "enif_get_local_pid", "env: *mut ErlNifEnv, arg1: ERL_NIF_TERM, pid: *mut ErlNifPid"},
    {"", "enif_keep_resource", "obj: *const c_void"},
    {"ERL_NIF_TERM", "enif_make_resource_binary", "arg1: *mut ErlNifEnv, obj: *const c_void, data: *const c_void, size: size_t"}
    ] ++

    case proplists:get_value(ulongsize, Opts) of
        8 -> [];
        4 ->
            [
                {"c_int", "enif_get_int64", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_longlong"},
                {"c_int", "enif_get_uint64", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulonglong"},
                {"ERL_NIF_TERM", "enif_make_int64", "env: *mut ErlNifEnv, i: c_longlong"},
                {"ERL_NIF_TERM", "enif_make_uint64", "env: *mut ErlNifEnv, i: c_ulonglong"}
            ]

    end ++ [
    {"c_int", "enif_is_exception", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_make_reverse_list", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, list: *mut ERL_NIF_TERM"},
    {"c_int", "enif_is_number", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"*mut c_void", "enif_dlopen", "lib: *const c_uchar, err_handler: Option<unsafe extern \"C\" fn (arg1: *mut c_void, arg2: *const c_uchar)>, err_arg: *mut c_void"},
    {"*mut c_void", "enif_dlsym", "handle: *mut c_void, symbol: *const c_uchar, err_handler: Option<unsafe extern \"C\" fn (arg1: *mut c_void, arg2: *const c_uchar)>, err_arg: *mut c_void"},
    {"c_int", "enif_consume_timeslice", "arg1: *mut ErlNifEnv, percent: c_int"},
    {"c_int", "enif_is_map", "env: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_get_map_size", "env: *mut ErlNifEnv, term: ERL_NIF_TERM, size: *mut size_t"},
    {"ERL_NIF_TERM", "enif_make_new_map", "env: *mut ErlNifEnv"},
    {"c_int", "enif_make_map_put", "env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM"},
    {"c_int", "enif_get_map_value", "env: *mut ErlNifEnv, map: ERL_NIF_TERM, key: ERL_NIF_TERM, value: *mut ERL_NIF_TERM"},
    {"c_int", "enif_make_map_update", "env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM"},
    {"c_int", "enif_make_map_remove", "env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM"},
    {"c_int", "enif_map_iterator_create", "env: *mut ErlNifEnv, map: ERL_NIF_TERM, iter: *mut ErlNifMapIterator, entry: ErlNifMapIteratorEntry"},
    {"", "enif_map_iterator_destroy", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator"},
    {"c_int", "enif_map_iterator_is_head", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator"},
    {"c_int", "enif_map_iterator_is_tail", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator"},
    {"c_int", "enif_map_iterator_next", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator"},
    {"c_int", "enif_map_iterator_prev", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator"},
    {"c_int", "enif_map_iterator_get_pair", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator, key: *mut ERL_NIF_TERM, value: *mut ERL_NIF_TERM"},
    {"ERL_NIF_TERM", "enif_schedule_nif", "env: *mut ErlNifEnv, fun_name: *const c_uchar, flags:c_int, fp: unsafe extern \"C\" fn(env: *mut ErlNifEnv, argc:c_int, argv:*const ERL_NIF_TERM) -> ERL_NIF_TERM, argc:c_int, argv:*const ERL_NIF_TERM"}
    ] ++


    case proplists:get_bool(exception, Opts) of
        true -> [
            {"c_int", "enif_has_pending_exception", "env: *mut ErlNifEnv, reason: *mut ERL_NIF_TERM"},
            {"ERL_NIF_TERM", "enif_raise_exception", "env: *mut ErlNifEnv, reason: ERL_NIF_TERM"}
        ];
        false -> []
    end ++



    case proplists:get_bool(getenv, Opts) of
        true -> [
            {"c_int", "enif_getenv", "key: *const c_uchar, value: *mut c_uchar, value_size: *mut size_t"}
        ];
        false -> []
    end ++


    case proplists:get_bool(time, Opts) of
        true -> [
            {"ErlNifTime", "enif_monotonic_time", "unit: ErlNifTimeUnit"},
            {"ErlNifTime", "enif_time_offset", "unit: ErlNifTimeUnit"},
            {"ErlNifTime", "enif_convert_time_unit", "time: ErlNifTime, from_unit: ErlNifTimeUnit, to_unit: ErlNifTimeUnit"}
        ];
        false -> []
    end ++


    case proplists:get_bool(dirty_schedulers, Opts) of
        true -> [{"c_int", "enif_is_on_dirty_scheduler", "env: *mut ErlNifEnv"}  ];
        false -> []
    end ++


    case proplists:get_bool(nif_2_11, Opts) of
        true -> [
            {"ERL_NIF_TERM", "enif_now_time", "env: *mut ErlNifEnv"},
            {"ERL_NIF_TERM", "enif_cpu_time", "env: *mut ErlNifEnv"},
            {"ERL_NIF_TERM", "enif_make_unique_integer", "env: *mut ErlNifEnv, properties: ErlNifUniqueInteger"},
            {"c_int", "enif_is_current_process_alive", "env: *mut ErlNifEnv"},
            {"c_int", "enif_is_process_alive", "env: *mut ErlNifEnv, pid: *const ErlNifPid"},
            {"c_int", "enif_is_port_alive", "env: *mut ErlNifEnv, port_id: *const ErlNifPort"},
            {"c_int", "enif_get_local_port", "env: *mut ErlNifEnv, term: ERL_NIF_TERM, port_id: *mut ErlNifPort"},
            {"c_int", "enif_term_to_binary", "env: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary"},
            {"usize", "enif_binary_to_term", "env: *mut ErlNifEnv, data: *const c_uchar, sz: usize, term: *mut ERL_NIF_TERM, opts: ErlNifBinaryToTerm"},
            {"c_int", "enif_port_command", "env: *mut ErlNifEnv, to_port: *const ErlNifPort, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM"},
            {"c_int", "enif_thread_type", ""},
            {"c_int", "enif_snprintf", "out: *mut c_char, size: usize, format: *const c_char, ..."}
        ];
        false -> []
    end ++

    case proplists:get_bool(nif_2_12, Opts) of
        true -> [
            {"c_int",                     "enif_select",               "env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *const c_void, pid: *const ErlNifPid, eref: ERL_NIF_TERM"},
            {"*const ErlNifResourceType", "enif_open_resource_type_x", "env: *mut ErlNifEnv, name_str: *const c_uchar, init: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags"},
            {"c_int",                     "enif_monitor_process",      "env: *mut ErlNifEnv, obj: *const c_void, pid: *const ErlNifPid, monitor: *mut ErlNifMonitor"},
            {"c_int",                     "enif_demonitor_process",    "env: *mut ErlNifEnv, obj: *const c_void,  monitor: *const ErlNifMonitor"},
            {"c_int",                     "enif_compare_monitors",     "monitor1: *const ErlNifMonitor, monitor2: *const ErlNifMonitor"},
            {"u64",                       "enif_hash",                 "hashtype: ErlNifHash, term: ERL_NIF_TERM, salt: u64"},
            {"c_int",                     "enif_whereis_pid",          "env: *mut ErlNifEnv, name: ERL_NIF_TERM, pid: *mut ErlNifPid"},
            {"c_int",                     "enif_whereis_port",         "env: *mut ErlNifEnv, name: ERL_NIF_TERM, port: *mut ErlNifPort"}
        ];
        false -> []
    end ++

    case proplists:get_bool(nif_2_13, Opts) of
        true -> [
            %% Skip iovec API for now (perhaps forever).
            %% Consider safer Rust iovec crates like https://crates.io/crates/iovec instead of this API.
            %% If anybody really does need this API in Rust, please file a bug.
            %% {"ErlNifIOQueue *",  "enif_ioq_create",     "ErlNifIOQueueOpts opts"},
            %% {"void",             "enif_ioq_destroy",    "ErlNifIOQueue *q"},
            %% {"int",              "enif_ioq_enq_binary", "ErlNifIOQueue *q, ErlNifBinary *bin, size_t skip"},
            %% {"int",              "enif_ioq_enqv",       "ErlNifIOQueue *q, ErlNifIOVec *iov, size_t skip"},
            %% {"size_t",           "enif_ioq_size",       "ErlNifIOQueue *q"},
            %% {"int",              "enif_ioq_deq",        "ErlNifIOQueue *q, size_t count, size_t *size"},
            %% {"SysIOVec*",        "enif_ioq_peek",       "ErlNifIOQueue *q, int *iovlen"},
            %% {"int",              "enif_inspect_iovec",  "ErlNifEnv *env, size_t max_length, ERL_NIF_TERM iovec_term, ERL_NIF_TERM *tail, ErlNifIOVec **iovec"},
            %% {"void",             "enif_free_iovec",     "ErlNifIOVec *iov"}
            {"", "dummy_enif_ioq_create",     ""},
            {"", "dummy_enif_ioq_destroy",    ""},
            {"", "dummy_enif_ioq_enq_binary", ""},
            {"", "dummy_enif_ioq_enqv",       ""},
            {"", "dummy_enif_ioq_size",       ""},
            {"", "dummy_enif_ioq_deq",        ""},
            {"", "dummy_enif_ioq_peek",       ""},
            {"", "dummy_enif_inspect_iovec",  ""},
            {"", "dummy_enif_free_iovec",     ""}
        ];
        false -> []
    end ++
    case proplists:get_bool(nif_2_14, Opts) of
        true -> [
            %% Skip iovec and synchronization APIs for now (perhaps forever).
            %% Consider safer Rust iovec crates like https://crates.io/crates/iovec instead of this API.
            %% If anybody really does need this API in Rust, please file a bug.
            % {"int",  "enif_ioq_peek_head",        "ErlNifEnv *env, ErlNifIOQueue *q, size_t *size, ERL_NIF_TERM *head"},
            % {"char*, "enif_mutex_name",           "ErlNifMutex*"},
            % {"char*, "enif_cond_name",            "ErlNifCond*"},
            % {"char*, "enif_rwlock_name",          "ErlNifRWLock*"},
            % {"char*, "enif_thread_name",          "ErlNifTid"},
            {"", "dummy_enif_ioq_peek_head", ""},
            {"", "dummy_enif_mutex_name",    ""},
            {"", "dummy_enif_cond_name",     ""},
            {"", "dummy_enif_rwlock_name",   ""},
            {"", "dummy_enif_thread_name",   ""},


            %% See format! and write!
            % {"int",  "enif_vfprintf",             "FILE*, const char *fmt, va_list"},
            % {"int",  "enif_vsnprintf",            "char*, size_t, const char *fmt, va_list"},
            {"", "dummy_enif_vfprintf",             ""},
            {"", "dummy_enif_vsnprintf",            ""},

            {"c_int", "enif_make_map_from_arrays", "env: *mut ErlNifEnv, keys: *const ERL_NIF_TERM, values: *const ERL_NIF_TERM, cnt: usize, map_out: *mut ERL_NIF_TERM"}
        ];
        false -> []
    end ++
    case proplists:get_bool(nif_2_15, Opts) of
        true -> [
            {"ErlNifTermType", "enif_term_type", "env: *mut ErlNifEnv, term: *const ERL_NIF_TERM"},

            {"c_int", "enif_is_pid_undefined", "pid: *const ErlNifPid"},
            {"", "enif_set_pid_undefined", "pid: *mut ErlNifPid"},
            {"ERL_NIF_TERM", "enif_make_monitor_term", "env: *mut ErlNifEnv, mon: *const ErlNifMonitor"}
        ];
        false -> []
    end.


main([UlongSizeT]) -> main([UlongSizeT,"nif_api.snippet"]);
main([UlongSizeT, Filename]) ->
    %% Round up all configuration options
    Version = (catch erlang:system_info(nif_version)),
    Opts = version_opts(Version) ++ ulong_opts(UlongSizeT) ++ dirty_scheduler_opts(Version),
    %% Generate API list
    Entries = api_list(Opts),
    %% Generate Rust code
    Rust = [
        nif_entry_options_rust(Opts),
        nif_version_rust(proplists:get_value(major, Opts), proplists:get_value(minor, Opts)),
        api_bindings_rust(erlang:system_info(system_architecture), Entries),
        int64_mappers_rust(proplists:get_value(ulongsize, Opts))
    ],
    %% And write it
    ok = file:write_file(Filename, Rust),
    ok.

nif_entry_options_rust(Opts) ->
    DirtySchedulerOpt = proplists:get_bool(dirty_scheduler_opt, Opts),
    case DirtySchedulerOpt of
        true -> "pub const ERL_NIF_ENTRY_OPTIONS: c_uint = ERL_NIF_DIRTY_NIF_OPTION;\n";
        false-> "pub const ERL_NIF_ENTRY_OPTIONS: c_uint = 0;\n"
    end.


nif_version_rust(Major, Minor) ->
    [io_lib:format("pub const NIF_MAJOR_VERSION: c_int = ~p;\n",   [Major]),
     io_lib:format("pub const NIF_MINOR_VERSION: c_int = ~p;\n\n", [Minor])].

api_bindings_rust("win32", Entries) ->
    [ "#[allow(dead_code)]\n",
      "#[derive(Copy, Clone)]\n",
      "pub struct TWinDynNifCallbacks {\n",
      [io_lib:format("    ~s: ~s,\n",[Name,fn_type(Params, Return)]) || {Return,Name,Params} <- Entries],
      "}\n\n",

      % The line below would be the "faithful" reproduction of the NIF Win API, but Rust
      % is currently not allowing statics to be uninitialized (1.3 beta).  Revisit this when
      % RFC911 is implemented (or some other mechanism)
      %"static mut WinDynNifCallbacks:TWinDynNifCallbacks = unsafe{std::mem::uninitialized()};\n\n",

      % The work-around is to use Option.  The problem here is that we have to do an unwrap() for
      % each API call which is extra work.
      "pub static mut WIN_DYN_NIF_CALLBACKS:Option<TWinDynNifCallbacks> = None;\n\n",

      [ [ io_lib:format("/// See [~s](http://www.erlang.org/doc/man/erl_nif.html#~s) in the Erlang docs.\n", [Name, Name]),
          case is_variadic(Params) of
            true ->
                io_lib:format("#[macro_export]\n"
                              "macro_rules! ~s {\n"
                              "    ( $( $arg:expr ),*  ) => { $crate::get_~s()($($arg),*) };\n"
                              "    ( $( $arg:expr ),+, ) => { ~s!($($arg),*) };\n"
                              "}\n\n"
                              "#[inline]\n"
                              "#[doc(hidden)]\n"
                              "pub unsafe fn get_~s() -> ~s {\n"
                              "    WIN_DYN_NIF_CALLBACKS.unchecked_unwrap().~s\n"
                              "}\n\n",
                              [Name,Name,Name,Name,fn_type(Params, Return),Name]);
            _ ->
                io_lib:format("#[inline]\n"
                              "pub unsafe fn ~s(~s)~s {\n"
                              "    (WIN_DYN_NIF_CALLBACKS.unchecked_unwrap().~s)(~s)\n"
                              "}\n\n",
                              [Name,Params,ret_type(Return),Name,strip_types_from_params(Params)])
          end] || {Return,Name,Params} <- Entries, not is_dummy(Name)
      ]
    ];

api_bindings_rust(_Arch, Entries) ->
    [
        "extern \"C\" {\n",
        [ io_lib:format("/// See [~s](http://www.erlang.org/doc/man/erl_nif.html#~s) in the Erlang docs.\n"
                        "pub fn ~s(~s)~s;\n",
                        [Name,Name,Name,Params,ret_type(Return)])
          || {Return,Name,Params} <- Entries, not is_dummy(Name), not is_variadic(Params)],
        "}\n\n",
        [ io_lib:format("extern \"C\" {\n"
                        "    #[doc(hidden)]\n"
                        "    #[link_name = \"~s\"]\n"
                        "    pub fn _~s(~s)~s;\n"
                        "}\n\n"
                        "/// See [~s](http://www.erlang.org/doc/man/erl_nif.html#~s) in the Erlang docs.\n"
                        "#[macro_export]\n"
                        "macro_rules! ~s {\n"
                        "    ( $( $arg:expr ),*  ) => { $crate::_~s($($arg),*) };\n"
                        "    ( $( $arg:expr ),+, ) => { ~s!($($arg),*) };\n"
                        "}\n\n",
                        [Name,Name,Params,ret_type(Return),Name,Name,Name,Name,Name])
          || {Return,Name,Params} <- Entries, not is_dummy(Name), is_variadic(Params)]
    ].

is_dummy([$d,$u,$m,$m,$y|_]) -> true;
is_dummy(_) -> false.

is_variadic("...") -> true;
is_variadic("") -> false;
is_variadic([_|T]) -> is_variadic(T).

fn_type(Params, "")     -> io_lib:format("extern \"C\" fn (~s)", [Params]);
fn_type(Params, Return) -> io_lib:format("extern \"C\" fn (~s) -> ~s", [Params, Return]).

ret_type("") -> "";
ret_type(Return) -> " -> " ++ Return.



%% Strip types from Rust function definition, example:
%% "arg1: *mut ErlNifEnv, i: c_uint" -> "arg1, i"
strip_types_from_params(Params) ->
    ParamsCleaned0 = re:replace(Params, " ", ""),  % strip spaces
    ParamsCleaned = re:replace(ParamsCleaned0, "\\(.*\\)", ""), % strip nested function params
    ParamsL = re:split(ParamsCleaned, ","),
    ArgsL = [ re:replace(Param, ":.*", "") || Param <- ParamsL ], % strip type info
    join(ArgsL, ", ").

join(List, Joiner) -> join([], List, Joiner).
join(Acc, [H], _Joiner) -> lists:reverse([H|Acc]);
join(Acc, [H|T], Joiner) -> join([Joiner, H|Acc], T, Joiner).

%% These functions are defined in the API above when sizeof(ulong)==8, or they map to
%% long/ulong functions when sizeof(ulong)==4.
int64_mappers_rust(4) ->
    "use std::os::raw::{c_ulonglong, c_longlong};";

int64_mappers_rust(8) -> join([
    "/// See [enif_make_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int64) at erlang.org",
    "#[inline]",
    "pub unsafe fn enif_make_int64(env: *mut ErlNifEnv, i: i64) -> ERL_NIF_TERM",
    "    { enif_make_long(env, i) }",
    "",
    "/// See [enif_make_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint64) at erlang.org",
    "#[inline]",
    "pub unsafe fn enif_make_uint64(env: *mut ErlNifEnv, i: u64) -> ERL_NIF_TERM",
    "    { enif_make_ulong(env, i) }",
    "",
    "/// See [enif_get_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int64) at erlang.org",
    "#[inline]",
    "pub unsafe fn enif_get_int64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int",
    "    { enif_get_long(env, term, ip) }",
    "",
    "/// See [enif_get_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint64) at erlang.org",
    "#[inline]",
    "pub unsafe fn enif_get_uint64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int",
    "    { enif_get_ulong(env, term, ip) }"],
    "\n").
