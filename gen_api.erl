

%% gen_api

%% The NIF version that this branch is coded for.
-define(NIF_MAJOR_VERSION, 2).
-define(NIF_MINOR_VERSION, 7).


-define(NIF_VERSION_STRING, "2.7").

%% The Erlang version this branch is coded for.  This is only used for build
%% error messages to help the user find the right branch.
-define(ERLANG_VERSION_STRING, "17.4").

%% api_list
%%
%% This is the API list that corresponds to the list found in erl_nif_api_funcs.h
%% Initial conversion provided by https://github.com/lavrin/erlang-rust-nif/issues/2
%%
%% Functions that are not supported for Rust must still be listed so that
%% the Windows callback struct keeps proper integrity.  Such functions
%% must begin with "dummy."
%%
api_list(Wordsize, HasDirtySchedulers) -> [

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
    {"ERL_NIF_TERM", "enif_make_atom", "env: *mut ErlNifEnv, name: *const c_char"},
    {"c_int", "enif_make_existing_atom", "env: *mut ErlNifEnv, name: *const c_char, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding"},

    %{"ERL_NIF_TERM", "enif_make_tuple", "env: *mut ErlNifEnv, cnt: c_uint, ..."},
    %{"ERL_NIF_TERM", "enif_make_list", "env: *mut ErlNifEnv, cnt: c_uint, ..."},
    {"", "dummy_enif_make_tuple", ""}, % Can't support variable arguments
    {"", "dummy_enif_make_list", ""}, % Can't support variable arguments

    {"ERL_NIF_TERM", "enif_make_list_cell", "env: *mut ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM"},
    {"ERL_NIF_TERM", "enif_make_string", "env: *mut ErlNifEnv, string: *const c_char, arg1: ErlNifCharEncoding"},
    {"ERL_NIF_TERM", "enif_make_ref", "env: *mut ErlNifEnv"},

    %% Skip threading API for now (perhaps forever)
    %% If anybody has a situation where they want to use this API instead of the very fine
    %% Rust API, please tell me.
    %%      {"*mut ErlNifMutex", "enif_mutex_create", "name: *mut c_char"},
    %%      {"", "enif_mutex_destroy", "mtx: *mut ErlNifMutex"},
    %%      {"c_int", "enif_mutex_trylock", "mtx: *mut ErlNifMutex"},
    %%      {"", "enif_mutex_lock", "mtx: *mut ErlNifMutex"},
    %%      {"", "enif_mutex_unlock", "mtx: *mut ErlNifMutex"},
    %%      {"*mut ErlNifCond", "enif_cond_create", "name: *mut c_char"},
    %%      {"", "enif_cond_destroy", "cnd: *mut ErlNifCond"},
    %%      {"", "enif_cond_signal", "cnd: *mut ErlNifCond"},
    %%      {"", "enif_cond_broadcast", "cnd: *mut ErlNifCond"},
    %%      {"", "enif_cond_wait", "cnd: *mut ErlNifCond, mtx: *mut ErlNifMutex"},
    %%      {"*mut ErlNifRWLock", "enif_rwlock_create", "name: *mut c_char"},
    %%      {"", "enif_rwlock_destroy", "rwlck: *mut ErlNifRWLock"},
    %%      {"c_int", "enif_rwlock_tryrlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_rlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_runlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"c_int", "enif_rwlock_tryrwlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_rwlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"", "enif_rwlock_rwunlock", "rwlck: *mut ErlNifRWLock"},
    %%      {"c_int", "enif_tsd_key_create", "name: *mut c_char, key: *mut ErlNifTSDKey"},
    %%      {"", "enif_tsd_key_destroy", "key: ErlNifTSDKey"},
    %%      {"", "enif_tsd_set", "key: ErlNifTSDKey, data: *mut c_void"},
    %%      {"*mut c_void", "enif_tsd_get", "key: ErlNifTSDKey"},
    %%      {"*mut ErlNifThreadOpts", "enif_thread_opts_create", "name: *mut c_char"},
    %%      {"", "enif_thread_opts_destroy", "opts: *mut ErlNifThreadOpts"},
    %%      {"c_int", "enif_thread_create", "name: *mut c_char, tid: *mut ErlNifTid, func: ::std::option::Option<extern \"C\" fn (arg1: *mut c_void) -> *mut c_void>, args: *mut c_void, opts: *mut ErlNifThreadOpts"},
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

    %{"c_int", "enif_fprintf", "filep: *mut c_void, format: *const c_char, ..."},
    {"", "dummy_enif_fprintf", ""}, % Can't support variable arguments

    {"c_int", "enif_inspect_iolist_as_binary", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary"},
    {"ERL_NIF_TERM", "enif_make_sub_binary", "arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, pos: size_t, size: size_t"},
    {"c_int", "enif_get_string", "arg1: *mut ErlNifEnv, list: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg2: ErlNifCharEncoding"},
    {"c_int", "enif_get_atom", "arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, buf: *mut c_char, len: c_uint, arg2: ErlNifCharEncoding"},
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
    {"*mut ErlNifResourceType", "enif_open_resource_type", "arg1: *mut ErlNifEnv, module_str: *const c_char, name_str: *const c_char, dtor: ::std::option::Option<extern \"C\" fn (arg1: *mut ErlNifEnv, arg2: *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags"},
    {"*mut c_void", "enif_alloc_resource", "_type: *mut ErlNifResourceType, size: size_t"},
    {"", "enif_release_resource", "obj: *mut c_void"},
    {"ERL_NIF_TERM", "enif_make_resource", "arg1: *mut ErlNifEnv, obj: *mut c_void"},
    {"c_int", "enif_get_resource", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, _type: *mut ErlNifResourceType, objp: *mut *mut c_void"},
    {"size_t", "enif_sizeof_resource", "obj: *mut c_void"},
    {"*mut c_uchar", "enif_make_new_binary", "arg1: *mut ErlNifEnv, size: size_t, termp: *mut ERL_NIF_TERM"},
    {"c_int", "enif_is_list", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_is_tuple", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_get_atom_length", "arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, len: *mut c_uint, arg2: ErlNifCharEncoding"},
    {"c_int", "enif_get_list_length", "env: *mut ErlNifEnv, term: ERL_NIF_TERM, len: *mut c_uint"},
    {"ERL_NIF_TERM", "enif_make_atom_len", "env: *mut ErlNifEnv, name: *const c_char, len: size_t"},
    {"c_int", "enif_make_existing_atom_len", "env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding"},
    {"ERL_NIF_TERM", "enif_make_string_len", "env: *mut ErlNifEnv, string: *const c_char, len: size_t, arg1: ErlNifCharEncoding"},
    {"*mut ErlNifEnv", "enif_alloc_env", ""},
    {"", "enif_free_env", "env: *mut ErlNifEnv"},
    {"", "enif_clear_env", "env: *mut ErlNifEnv"},
    {"c_int", "enif_send", "env: *mut ErlNifEnv, to_pid: *const ErlNifPid, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM"},
    {"ERL_NIF_TERM", "enif_make_copy", "dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM"},
    {"*mut ErlNifPid", "enif_self", "caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid"},
    {"c_int", "enif_get_local_pid", "env: *mut ErlNifEnv, arg1: ERL_NIF_TERM, pid: *mut ErlNifPid"},
    {"", "enif_keep_resource", "obj: *mut c_void"},
    {"ERL_NIF_TERM", "enif_make_resource_binary", "arg1: *mut ErlNifEnv, obj: *mut c_void, data: *const c_void, size: size_t"}
    ] ++

    case Wordsize of
        8 -> [];
        4 ->
            [
                {"c_int", "enif_get_int64", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut int64_t"},
                {"c_int", "enif_get_uint64", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut uint64_t"},
                {"ERL_NIF_TERM", "enif_make_int64", "env: *mut ErlNifEnv, i: int64_t"},
                {"ERL_NIF_TERM", "enif_make_uint64", "env: *mut ErlNifEnv, i: uint64_t"}
            ]

    end ++ [
    {"c_int", "enif_is_exception", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"c_int", "enif_make_reverse_list", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, list: *mut ERL_NIF_TERM"},
    {"c_int", "enif_is_number", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM"},
    {"*mut c_void", "enif_dlopen", "lib: *const c_char, err_handler: ::std::option::Option<extern \"C\" fn (arg1: *mut c_void, arg2: *const c_char)>, err_arg: *mut c_void"},
    {"*mut c_void", "enif_dlsym", "handle: *mut c_void, symbol: *const c_char, err_handler: ::std::option::Option<extern \"C\" fn (arg1: *mut c_void, arg2: *const c_char)>, err_arg: *mut c_void"},
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
    {"c_int", "enif_map_iterator_get_pair", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator, key: *mut ERL_NIF_TERM, value: *mut ERL_NIF_TERM"}
    ] ++

    case HasDirtySchedulers of
        true -> [{"c_int", "enif_is_on_dirty_scheduler", "env: *mut ErlNifEnv"}  ];
        false -> []
    end.


main([]) -> main(["."]);
main([OutputDir]) ->

    %% Collect and check erlang configuration
    check_nif_version(),
    Wordsize = erlang:system_info(wordsize),
    HasDirtySchedulers = case catch erlang:system_info(dirty_cpu_schedulers) of
                             _X when is_integer(_X) -> true;
                             _ -> false
                         end,

    %% Generate API list
%%     Bin = api_bin(Wordsize, HasDirtySchedulers),
%%     Entries = extract_all_entries(Bin),
    Entries = api_list(Wordsize, HasDirtySchedulers),

    %% Emit bindings
    emit_nif_version(OutputDir),

    case erlang:system_info(system_architecture) of
        "win32" -> emit_windows_bindings(Entries, OutputDir);
        _ -> emit_unix_bindings(Entries, OutputDir)
    end,

    ok.



emit_unix_bindings(Entries, OutputDir) ->
    Filename = filename:join(OutputDir, "nif_api.snippet"),
    Data = [ "extern \"C\" {\n",
             [ case Return of
                   "" ->
                       io_lib:format("pub fn ~s(~s);\n",[Name,Params]);
                   _ ->
                       io_lib:format("pub fn ~s(~s) -> ~s;\n",[Name,Params,Return])
               end || {Return,Name,Params} <- Entries, not is_dummy(Name)],
             "}\n"
    ],
    file:write_file(Filename, Data).

is_dummy([$d,$u,$m,$m,$y|_]) -> true;
is_dummy(_) -> false.

emit_nif_version(OutputDir) ->
    Filename = filename:join(OutputDir, "nif_versions.snippet"),
    Data = [io_lib:format("const NIF_MAJOR_VERSION: c_int = ~p;\n", [?NIF_MAJOR_VERSION]),
            io_lib:format("const NIF_MINOR_VERSION: c_int = ~p;\n", [?NIF_MINOR_VERSION])],

    file:write_file(Filename, Data).

%% emit_nif_version(OutputDir) ->
%%     Filename = filename:join(OutputDir, "nif_versions.snippet"),
%%     Data = ["mod nif_versions {\n",
%%             "use libc::c_int;\n",
%%             io_lib:format("const NIF_MAJOR_VERSION: c_int = ~p;\n", [?NIF_MAJOR_VERSION]),
%%             io_lib:format("const NIF_MINOR_VERSION: c_int = ~p;\n", [?NIF_MINOR_VERSION]),
%%             "}\n"],
%%
%%     file:write_file(Filename, Data).
%%

emit_windows_bindings(Entries, OutputDir) ->
    io:format("Windows binding generation not yet implemented.\n"),
    halt(1),

    %% That said, here's a sketch of how Windows bindings are going to work...
    Filename = filename:join(OutputDir, "nif_api.snippet"),

    Data = ["struct TWinDynNifCallbacks {\n",
            [ case Return of
                  "" ->
                      io:format("~s: fn(~s),\n",[Name,Params]);
                  _ ->
                      io:format("~s: fn(~s) -> ~s,\n",[Name,Params,Return])
              end || {Return,Name,Params} <- Entries],

            "}\n\n",
            "static TWinDynNifCallbacks: WinDynNifCallbacks;\n\n",

            [ case Return of
                  "" ->
                      io:format("#[inline] pub fn ~s(~s) {WinDynNifCallbacks.~s~s}\n",[Name,Params,Name,Params]);
                  _ ->
                      io:format("#[inline] pub fn ~s(~s) -> ~s {WinDynNifCallbacks.~s~s}\n",[Name,Params,Return,Name,Params])
              end || {Return,Name,Params} <- Entries, not is_dummy(Name)],
            "\n\n"],

    file:write_file(Filename, Data).



check_nif_version() ->
    case catch erlang:system_info(nif_version) of
        ?NIF_VERSION_STRING -> ok;
        _ ->
            io:format("Wrong Erlang version.\nThis branch is coded for Erlang ~s and NIF version ~s\n",
                      [?ERLANG_VERSION_STRING, ?NIF_VERSION_STRING]),
            halt(1)
    end.





%% emit_debug_bindings(Entries) ->
%%     [io:format("{~p, ~p, ~p},\n",[binary_to_list(Return), binary_to_list(Name), binary_to_list(Params)]) ||
%%         {Name,Params,Return} <- Entries].
%% extract_all_entries(Bin0) ->
%%     extract_all_entries(Bin0, []).
%%
%% extract_all_entries(<<>>, Acc) -> lists:reverse(Acc);
%% extract_all_entries(Bin0, Acc) ->
%%     {R, Bin1} = extract_entry(Bin0),
%%     extract_all_entries(Bin1, [R|Acc]).
%%
%%
%%
%% extract_entry(Bin0) ->
%%
%%     {Name, <<_OpenParen,Bin1/binary>>} = regex_split3(Bin0, "\\s*pub fn\\s+(.*)\\("),
%%     {Params0, Bin2} = get_matching_paren(Bin1),
%%     Params1 = iolist_to_binary(re:replace(Params0, "\\s+", " ", [global])),
%%     {Return, Bin3} = regex_split3(Bin2, "\\s*(?:->)?\\s*(.*)\\s*;"),
%%     {_Ate, Bin4} = regex_split3(Bin3, "(\\s*;\\s)"),  % eat garbage
%%     {{Name, Params1, Return}, Bin4}.
%%
%% regex_split3(Bin, Re) ->
%%     {match, [{Loc, Len}]} = re:run(Bin, Re, [multiline, {capture, all_but_first}]),
%%     { binary:part(Bin, Loc, Len),
%%       binary:part(Bin, Loc+Len, size(Bin)-(Loc+Len))}.
%%
%% get_matching_paren(Bin) -> get_matching_paren(Bin, [], 1).
%%
%% get_matching_paren(<<"(", Rest/binary>>, Acc, Depth) ->
%%     get_matching_paren(Rest, ["("|Acc], Depth+1);
%%
%% get_matching_paren(<<")", Rest/binary>>, Acc, 1) ->
%%     { iolist_to_binary(lists:reverse(Acc)), Rest };
%%
%% get_matching_paren(<<")", Rest/binary>>, Acc, Depth) ->
%%     get_matching_paren(Rest, [")"|Acc], Depth-1);
%%
%% get_matching_paren(<<H, Rest/binary>>, Acc, Depth) ->
%%     get_matching_paren(Rest, [H|Acc], Depth).
%%
%% api_bin(_Wordsize, _HasDirtySchedulers) -> iolist_to_binary([
%%                                                                 <<"
%%     pub fn enif_priv_data(arg1: *mut ErlNifEnv) -> *mut c_void;
%%     pub fn enif_alloc(size: size_t) -> *mut c_void;
%%     pub fn enif_free(ptr: *mut c_void);
%%     pub fn enif_is_atom(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_is_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_is_ref(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_inspect_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM,
%%                                bin: *mut ErlNifBinary) -> c_int;
%%     pub fn enif_alloc_binary(size: size_t, bin: *mut ErlNifBinary) ->
%%      c_int;
%%     pub fn enif_realloc_binary(bin: *mut ErlNifBinary, size: size_t) ->
%%      c_int;
%%     pub fn enif_release_binary(bin: *mut ErlNifBinary);
%%     pub fn enif_get_int(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                         ip: *mut c_int) -> c_int;
%%     pub fn enif_get_ulong(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                           ip: *mut c_ulong) -> c_int;
%%     pub fn enif_get_double(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                            dp: *mut c_double) -> c_int;
%%     pub fn enif_get_list_cell(env: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                               head: *mut ERL_NIF_TERM,
%%                               tail: *mut ERL_NIF_TERM) -> c_int;
%%     pub fn enif_get_tuple(env: *mut ErlNifEnv, tpl: ERL_NIF_TERM,
%%                           arity: *mut c_int,
%%                           array: *mut *const ERL_NIF_TERM) -> c_int;
%%     pub fn enif_is_identical(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_compare(lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_make_binary(env: *mut ErlNifEnv, bin: *mut ErlNifBinary) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_badarg(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
%%     pub fn enif_make_int(env: *mut ErlNifEnv, i: c_int) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_ulong(env: *mut ErlNifEnv, i: c_ulong) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_double(env: *mut ErlNifEnv, d: c_double) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_atom(env: *mut ErlNifEnv, name: *const c_char) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_existing_atom(env: *mut ErlNifEnv,
%%                                    name: *const c_char,
%%                                    atom: *mut ERL_NIF_TERM,
%%                                    arg1: ErlNifCharEncoding) -> c_int;
%%     pub fn enif_make_tuple(env: *mut ErlNifEnv, cnt: c_uint, ...) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_list(env: *mut ErlNifEnv, cnt: c_uint, ...) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_list_cell(env: *mut ErlNifEnv, car: ERL_NIF_TERM,
%%                                cdr: ERL_NIF_TERM) -> ERL_NIF_TERM;
%%     pub fn enif_make_string(env: *mut ErlNifEnv,
%%                             string: *const c_char,
%%                             arg1: ErlNifCharEncoding) -> ERL_NIF_TERM;
%%     pub fn enif_make_ref(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
%%     pub fn enif_mutex_create(name: *mut c_char) -> *mut ErlNifMutex;
%%     pub fn enif_mutex_destroy(mtx: *mut ErlNifMutex);
%%     pub fn enif_mutex_trylock(mtx: *mut ErlNifMutex) -> c_int;
%%     pub fn enif_mutex_lock(mtx: *mut ErlNifMutex);
%%     pub fn enif_mutex_unlock(mtx: *mut ErlNifMutex);
%%     pub fn enif_cond_create(name: *mut c_char) -> *mut ErlNifCond;
%%     pub fn enif_cond_destroy(cnd: *mut ErlNifCond);
%%     pub fn enif_cond_signal(cnd: *mut ErlNifCond);
%%     pub fn enif_cond_broadcast(cnd: *mut ErlNifCond);
%%     pub fn enif_cond_wait(cnd: *mut ErlNifCond, mtx: *mut ErlNifMutex);
%%     pub fn enif_rwlock_create(name: *mut c_char) -> *mut ErlNifRWLock;
%%     pub fn enif_rwlock_destroy(rwlck: *mut ErlNifRWLock);
%%     pub fn enif_rwlock_tryrlock(rwlck: *mut ErlNifRWLock) -> c_int;
%%     pub fn enif_rwlock_rlock(rwlck: *mut ErlNifRWLock);
%%     pub fn enif_rwlock_runlock(rwlck: *mut ErlNifRWLock);
%%     pub fn enif_rwlock_tryrwlock(rwlck: *mut ErlNifRWLock) -> c_int;
%%     pub fn enif_rwlock_rwlock(rwlck: *mut ErlNifRWLock);
%%     pub fn enif_rwlock_rwunlock(rwlck: *mut ErlNifRWLock);
%%     pub fn enif_tsd_key_create(name: *mut c_char,
%%                                key: *mut ErlNifTSDKey) -> c_int;
%%     pub fn enif_tsd_key_destroy(key: ErlNifTSDKey);
%%     pub fn enif_tsd_set(key: ErlNifTSDKey, data: *mut c_void);
%%     pub fn enif_tsd_get(key: ErlNifTSDKey) -> *mut c_void;
%%     pub fn enif_thread_opts_create(name: *mut c_char) ->
%%      *mut ErlNifThreadOpts;
%%     pub fn enif_thread_opts_destroy(opts: *mut ErlNifThreadOpts);
%%     pub fn enif_thread_create(name: *mut c_char, tid: *mut ErlNifTid,
%%                               func:
%%                                   ::std::option::Option<extern \"C\" fn
%%                                                             (arg1:
%%                                                                  *mut c_void)
%%                                                             ->
%%                                                                 *mut c_void>,
%%                               args: *mut c_void,
%%                               opts: *mut ErlNifThreadOpts) -> c_int;
%%     pub fn enif_thread_self() -> ErlNifTid;
%%     pub fn enif_equal_tids(tid1: ErlNifTid, tid2: ErlNifTid) -> c_int;
%%     pub fn enif_thread_exit(resp: *mut c_void);
%%     pub fn enif_thread_join(arg1: ErlNifTid, respp: *mut *mut c_void)
%%      -> c_int;
%%     pub fn enif_realloc(ptr: *mut c_void, size: size_t) ->
%%      *mut c_void;
%%     pub fn enif_system_info(sip: *mut ErlNifSysInfo, si_size: size_t);
%%     pub fn enif_fprintf(filep: *mut c_void,
%%                         format: *const c_char, ...) -> c_int;
%%     pub fn enif_inspect_iolist_as_binary(arg1: *mut ErlNifEnv,
%%                                          term: ERL_NIF_TERM,
%%                                          bin: *mut ErlNifBinary) ->
%%      c_int;
%%     pub fn enif_make_sub_binary(arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM,
%%                                 pos: size_t, size: size_t) -> ERL_NIF_TERM;
%%     pub fn enif_get_string(arg1: *mut ErlNifEnv, list: ERL_NIF_TERM,
%%                            buf: *mut c_char, len: c_uint,
%%                            arg2: ErlNifCharEncoding) -> c_int;
%%     pub fn enif_get_atom(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM,
%%                          buf: *mut c_char, len: c_uint,
%%                          arg2: ErlNifCharEncoding) -> c_int;
%%     pub fn enif_is_fun(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_is_pid(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_is_port(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_get_uint(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                          ip: *mut c_uint) -> c_int;
%%     pub fn enif_get_long(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                          ip: *mut c_long) -> c_int;
%%     pub fn enif_make_uint(arg1: *mut ErlNifEnv, i: c_uint) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_long(arg1: *mut ErlNifEnv, i: c_long) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_tuple_from_array(arg1: *mut ErlNifEnv,
%%                                       arr: *const ERL_NIF_TERM,
%%                                       cnt: c_uint) -> ERL_NIF_TERM;
%%     pub fn enif_make_list_from_array(arg1: *mut ErlNifEnv,
%%                                      arr: *const ERL_NIF_TERM,
%%                                      cnt: c_uint) -> ERL_NIF_TERM;
%%     pub fn enif_is_empty_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_open_resource_type(arg1: *mut ErlNifEnv,
%%                                    module_str: *const c_char,
%%                                    name_str: *const c_char,
%%                                    dtor:
%%                                        ::std::option::Option<extern \"C\" fn
%%                                                                  (arg1:
%%                                                                       *mut ErlNifEnv,
%%                                                                   arg2:
%%                                                                       *mut c_void)>,
%%                                    flags: ErlNifResourceFlags,
%%                                    tried: *mut ErlNifResourceFlags) ->
%%      *mut ErlNifResourceType;
%%     pub fn enif_alloc_resource(_type: *mut ErlNifResourceType, size: size_t)
%%      -> *mut c_void;
%%     pub fn enif_release_resource(obj: *mut c_void);
%%     pub fn enif_make_resource(arg1: *mut ErlNifEnv, obj: *mut c_void)
%%      -> ERL_NIF_TERM;
%%     pub fn enif_get_resource(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                              _type: *mut ErlNifResourceType,
%%                              objp: *mut *mut c_void) -> c_int;
%%     pub fn enif_sizeof_resource(obj: *mut c_void) -> size_t;
%%     pub fn enif_make_new_binary(arg1: *mut ErlNifEnv, size: size_t,
%%                                 termp: *mut ERL_NIF_TERM) ->
%%      *mut c_uchar;
%%     pub fn enif_is_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_is_tuple(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_get_atom_length(arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM,
%%                                 len: *mut c_uint,
%%                                 arg2: ErlNifCharEncoding) -> c_int;
%%     pub fn enif_get_list_length(env: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                                 len: *mut c_uint) -> c_int;
%%     pub fn enif_make_atom_len(env: *mut ErlNifEnv,
%%                               name: *const c_char, len: size_t) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_make_existing_atom_len(env: *mut ErlNifEnv,
%%                                        name: *const c_char,
%%                                        len: size_t, atom: *mut ERL_NIF_TERM,
%%                                        arg1: ErlNifCharEncoding) ->
%%      c_int;
%%     pub fn enif_make_string_len(env: *mut ErlNifEnv,
%%                                 string: *const c_char, len: size_t,
%%                                 arg1: ErlNifCharEncoding) -> ERL_NIF_TERM;
%%     pub fn enif_alloc_env() -> *mut ErlNifEnv;
%%     pub fn enif_free_env(env: *mut ErlNifEnv);
%%     pub fn enif_clear_env(env: *mut ErlNifEnv);
%%     pub fn enif_send(env: *mut ErlNifEnv, to_pid: *const ErlNifPid,
%%                      msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_make_copy(dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM) ->
%%      ERL_NIF_TERM;
%%     pub fn enif_self(caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid) ->
%%      *mut ErlNifPid;
%%     pub fn enif_get_local_pid(env: *mut ErlNifEnv, arg1: ERL_NIF_TERM,
%%                               pid: *mut ErlNifPid) -> c_int;
%%     pub fn enif_keep_resource(obj: *mut c_void);
%%     pub fn enif_make_resource_binary(arg1: *mut ErlNifEnv,
%%                                      obj: *mut c_void,
%%                                      data: *const c_void,
%%                                      size: size_t) -> ERL_NIF_TERM;
%%     pub fn enif_is_exception(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_make_reverse_list(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                                   list: *mut ERL_NIF_TERM) -> c_int;
%%     pub fn enif_is_number(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_dlopen(lib: *const c_char,
%%                        err_handler:
%%                            ::std::option::Option<extern \"C\" fn
%%                                                      (arg1:
%%                                                           *mut c_void,
%%                                                       arg2:
%%                                                           *const c_char)>,
%%                        err_arg: *mut c_void) -> *mut c_void;
%%     pub fn enif_dlsym(handle: *mut c_void,
%%                       symbol: *const c_char,
%%                       err_handler:
%%                           ::std::option::Option<extern \"C\" fn
%%                                                     (arg1:
%%                                                          *mut c_void,
%%                                                      arg2:
%%                                                          *const c_char)>,
%%                       err_arg: *mut c_void) -> *mut c_void;
%%     pub fn enif_consume_timeslice(arg1: *mut ErlNifEnv,
%%                                   percent: c_int) -> c_int;
%%     pub fn enif_is_map(env: *mut ErlNifEnv, term: ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_get_map_size(env: *mut ErlNifEnv, term: ERL_NIF_TERM,
%%                              size: *mut size_t) -> c_int;
%%     pub fn enif_make_new_map(env: *mut ErlNifEnv) -> ERL_NIF_TERM;
%%     pub fn enif_make_map_put(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM,
%%                              key: ERL_NIF_TERM, value: ERL_NIF_TERM,
%%                              map_out: *mut ERL_NIF_TERM) -> c_int;
%%     pub fn enif_get_map_value(env: *mut ErlNifEnv, map: ERL_NIF_TERM,
%%                               key: ERL_NIF_TERM, value: *mut ERL_NIF_TERM) ->
%%      c_int;
%%     pub fn enif_make_map_update(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM,
%%                                 key: ERL_NIF_TERM, value: ERL_NIF_TERM,
%%                                 map_out: *mut ERL_NIF_TERM) -> c_int;
%%     pub fn enif_make_map_remove(env: *mut ErlNifEnv, map_in: ERL_NIF_TERM,
%%                                 key: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM)
%%      -> c_int;
%%     pub fn enif_map_iterator_create(env: *mut ErlNifEnv, map: ERL_NIF_TERM,
%%                                     iter: *mut ErlNifMapIterator,
%%                                     entry: ErlNifMapIteratorEntry) ->
%%      c_int;
%%     pub fn enif_map_iterator_destroy(env: *mut ErlNifEnv,
%%                                      iter: *mut ErlNifMapIterator);
%%     pub fn enif_map_iterator_is_head(env: *mut ErlNifEnv,
%%                                      iter: *mut ErlNifMapIterator) ->
%%      c_int;
%%     pub fn enif_map_iterator_is_tail(env: *mut ErlNifEnv,
%%                                      iter: *mut ErlNifMapIterator) ->
%%      c_int;
%%     pub fn enif_map_iterator_next(env: *mut ErlNifEnv,
%%                                   iter: *mut ErlNifMapIterator) ->
%%      c_int;
%%     pub fn enif_map_iterator_prev(env: *mut ErlNifEnv,
%%                                   iter: *mut ErlNifMapIterator) ->
%%      c_int;
%%     pub fn enif_map_iterator_get_pair(env: *mut ErlNifEnv,
%%                                       iter: *mut ErlNifMapIterator,
%%                                       key: *mut ERL_NIF_TERM,
%%                                       value: *mut ERL_NIF_TERM) ->
%%      c_int;
%% ">>]).
