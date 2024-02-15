// build.rs
//
// Generate the NIF APIs that will be built in `src/rustler_sys_api.rs`.
//

use regex::Regex;
use std::fmt::Write;
use std::path::Path;
use std::{env, fs};

pub const MIN_SUPPORTED_VERSION: (u32, u32) = (2, 14);
pub const MAX_SUPPORTED_VERSION: (u32, u32) = (2, 17);

const SNIPPET_NAME: &str = "nif_api.snippet";

trait ApiBuilder {
    fn func(&mut self, ret: &str, name: &str, args: &str);
    fn variadic_func(&mut self, ret: &str, name: &str, args: &str);
    fn dummy(&mut self, name: &str);
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum OsFamily {
    Unix,
    Win,
}

pub struct GenerateOptions {
    pub ulong_size: usize,
    pub nif_version: (u32, u32),
    pub target_family: OsFamily,
}

fn write_ret(out: &mut String, ret: &str) {
    if !ret.is_empty() {
        write!(out, " -> {}", ret).unwrap();
    }
}

fn write_fn_type(out: &mut String, args: &str, ret: &str) {
    write!(out, "extern \"C\" fn ({})", args).unwrap();
    write_ret(out, ret);
}

fn write_variadic_fn_type(out: &mut String, args: &str, ret: &str) {
    write!(out, "extern \"C\" fn ({}, ...)", args).unwrap();
    write_ret(out, ret);
}

pub struct BasicApiBuilder<'a>(&'a mut String);

impl<'a> ApiBuilder for BasicApiBuilder<'a> {
    fn func(&mut self, ret: &str, name: &str, args: &str) {
        writeln!(self.0, "extern \"C\" {{").unwrap();
        writeln!(
            self.0,
            "    /// See [{}](http://www.erlang.org/doc/man/erl_nif.html#{}) in the Erlang docs.",
            name, name
        )
        .unwrap();

        write!(self.0, "    pub fn {}({})", name, args).unwrap();
        write_ret(self.0, ret);
        writeln!(self.0, ";").unwrap();

        writeln!(self.0, "}}").unwrap();
    }
    fn variadic_func(&mut self, ret: &str, name: &str, args: &str) {
        writeln!(self.0, "extern \"C\" {{").unwrap();
        writeln!(self.0, "    #[doc(hidden)]").unwrap();
        writeln!(self.0, "    #[link_name = \"{}\"]", name).unwrap();

        write!(self.0, "    pub fn _{}({}, ...)", name, args).unwrap();
        write_ret(self.0, ret);
        writeln!(self.0, ";").unwrap();

        writeln!(self.0, "}}\n").unwrap();

        writeln!(
            self.0,
            "/// See [{}](http://www.erlang.org/doc/man/erl_nif.html#{}) in the Erlang docs.",
            name, name
        )
        .unwrap();
        writeln!(self.0, "#[macro_export]").unwrap();
        writeln!(self.0, "macro_rules! {} {{", name).unwrap();
        writeln!(
            self.0,
            "    ( $( $arg:expr ),*  ) => {{ $crate::_{}($($arg),*) }};",
            name
        )
        .unwrap();
        writeln!(
            self.0,
            "    ( $( $arg:expr ),+, ) => {{ {}!($($arg),*) }};",
            name
        )
        .unwrap();
        writeln!(self.0, "}}\n").unwrap();
    }
    fn dummy(&mut self, _name: &str) {}
}

pub struct WinCallbacksApiBuilder<'a>(&'a mut String);
impl<'a> ApiBuilder for WinCallbacksApiBuilder<'a> {
    fn func(&mut self, ret: &str, name: &str, args: &str) {
        write!(self.0, "    {}: ", name).unwrap();
        write_fn_type(self.0, args, ret);
        writeln!(self.0, ",").unwrap();
    }
    fn variadic_func(&mut self, ret: &str, name: &str, args: &str) {
        write!(self.0, "    {}: ", name).unwrap();
        write_variadic_fn_type(self.0, args, ret);
        writeln!(self.0, ",").unwrap();
    }
    fn dummy(&mut self, name: &str) {
        write!(self.0, "    {}: ", name).unwrap();
        write_fn_type(self.0, "", "");
        writeln!(self.0, ",").unwrap();
    }
}

pub struct WinForwardersApiBuilder<'a>(&'a mut String);
impl<'a> ApiBuilder for WinForwardersApiBuilder<'a> {
    fn func(&mut self, ret: &str, name: &str, args: &str) {
        // This regex takes a list of args with types and return only the name of the args.
        //
        // Examples:
        // "arg1: *mut ErlNifEnv, i: c_uint" -> "arg1, i"
        // "arg1: Option<unsafe extern "C" fn (my_arg: *mut c_void)>, i: c_uint" -> "arg1, i"
        // "fp: unsafe extern "C" fn(env: *mut ErlNifEnv) -> ERL_NIF_TERM, argc:c_int" -> "fp, argc"
        let args_re = Regex::new(r#"(?P<arg>[a-z0-9_]*[^:]):(?:[a-zA-Z\s]+<[^>]*>|\s?unsafe extern "C" fn\([^)]*\)[^,]*|[^,]*)"#).unwrap();
        let args_names = args_re.replace_all(args, "$arg");

        writeln!(
            self.0,
            "/// See [{}](http://www.erlang.org/doc/man/erl_nif.html#{}) in the Erlang docs.",
            name, name
        )
        .unwrap();
        writeln!(self.0, "#[inline]").unwrap();
        writeln!(self.0, "pub unsafe fn {}({})", name, args).unwrap();
        write_ret(self.0, ret);
        writeln!(self.0, "{{").unwrap();
        writeln!(
            self.0,
            "    (WIN_DYN_NIF_CALLBACKS.unchecked_unwrap().{})({})",
            name, args_names
        )
        .unwrap();
        writeln!(self.0, "}}\n").unwrap();
    }
    fn variadic_func(&mut self, ret: &str, name: &str, args: &str) {
        writeln!(self.0, "#[macro_export]").unwrap();
        writeln!(self.0, "macro_rules! {} {{", name).unwrap();
        writeln!(
            self.0,
            "    ( $( $arg:expr ),* ) => {{ $crate::get_{}()($($arg),*) }};",
            name
        )
        .unwrap();
        writeln!(
            self.0,
            "    ( $( $arg:expr ),+, ) => {{ {}!($($arg),*) }};",
            name
        )
        .unwrap();
        writeln!(self.0, "}}\n").unwrap();

        write!(self.0, "pub unsafe fn get_{}() -> ", name).unwrap();
        write_variadic_fn_type(self.0, args, ret);
        writeln!(self.0, " {{").unwrap();
        writeln!(
            self.0,
            "    WIN_DYN_NIF_CALLBACKS.unchecked_unwrap().{}",
            name
        )
        .unwrap();
        writeln!(self.0, "}}\n").unwrap();
    }
    fn dummy(&mut self, _name: &str) {}
}

fn generate(opts: &GenerateOptions) -> String {
    let mut out = String::new();

    writeln!(
        out,
        "pub const ERL_NIF_ENTRY_OPTIONS: c_uint = ERL_NIF_DIRTY_NIF_OPTION;"
    )
    .unwrap();
    writeln!(
        out,
        "pub const NIF_MAJOR_VERSION: c_int = {};",
        opts.nif_version.0
    )
    .unwrap();
    writeln!(
        out,
        "pub const NIF_MINOR_VERSION: c_int = {};",
        opts.nif_version.1
    )
    .unwrap();

    // Basic
    if opts.target_family == OsFamily::Win {
        writeln!(out, "#[allow(dead_code)]").unwrap();
        writeln!(out, "#[derive(Copy, Clone)]").unwrap();
        writeln!(out, "pub struct TWinDynNifCallbacks {{").unwrap();
        build_api(&mut WinCallbacksApiBuilder(&mut out), opts);
        writeln!(out, "}}").unwrap();

        // The line below would be the "faithful" reproduction of the NIF Win API, but Rust
        // is currently not allowing statics to be uninitialized (1.3 beta).  Revisit this when
        // RFC911 is implemented (or some other mechanism)
        // writeln!(out, "pub static mut WIN_DYN_NIF_CALLBACKS: TWinDynNifCallbacks = unsafe {{ std::mem::uninitialized() }};\n").unwrap();

        // The work-around is to use Option.  The problem here is that we have to do an unwrap() for
        // each API call which is extra work.
        writeln!(
            out,
            "pub static mut WIN_DYN_NIF_CALLBACKS:Option<TWinDynNifCallbacks> = None;\n"
        )
        .unwrap();

        build_api(&mut WinForwardersApiBuilder(&mut out), opts);
    } else {
        build_api(&mut BasicApiBuilder(&mut out), opts);
    }

    if opts.ulong_size == 4 {
        writeln!(out, "use std::os::raw::{{c_ulonglong, c_longlong}};").unwrap();
    } else {
        write!(out, r#"
/// See [enif_make_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_int64) at erlang.org
#[inline]
pub unsafe fn enif_make_int64(env: *mut ErlNifEnv, i: i64) -> ERL_NIF_TERM
    {{ enif_make_long(env, i) }}

/// See [enif_make_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_make_uint64) at erlang.org
#[inline]
pub unsafe fn enif_make_uint64(env: *mut ErlNifEnv, i: u64) -> ERL_NIF_TERM
    {{ enif_make_ulong(env, i) }}

/// See [enif_get_int64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_int64) at erlang.org
#[inline]
pub unsafe fn enif_get_int64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int
    {{ enif_get_long(env, term, ip) }}

/// See [enif_get_uint64](http://www.erlang.org/doc/man/erl_nif.html#enif_get_uint64) at erlang.org
#[inline]
pub unsafe fn enif_get_uint64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int
    {{ enif_get_ulong(env, term, ip) }}
        "#).unwrap();
    }

    out
}

fn build_api(b: &mut dyn ApiBuilder, opts: &GenerateOptions) {
    b.func("*mut c_void", "enif_priv_data", "arg1: *mut ErlNifEnv");
    b.func("*mut c_void", "enif_alloc", "size: size_t");
    b.func("", "enif_free", "ptr: *mut c_void");
    b.func(
        "c_int",
        "enif_is_atom",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_is_binary",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_is_ref",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_inspect_binary",
        "arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, bin: *mut ErlNifBinary",
    );
    b.func(
        "c_int",
        "enif_alloc_binary",
        "size: size_t, bin: *mut ErlNifBinary",
    );
    b.func(
        "c_int",
        "enif_realloc_binary",
        "bin: *mut ErlNifBinary, size: size_t",
    );
    b.func("", "enif_release_binary", "bin: *mut ErlNifBinary");
    b.func(
        "c_int",
        "enif_get_int",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_int",
    );
    b.func(
        "c_int",
        "enif_get_ulong",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulong",
    );
    b.func(
        "c_int",
        "enif_get_double",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, dp: *mut c_double",
    );
    b.func(
        "c_int",
        "enif_get_list_cell",
        "env: *mut ErlNifEnv, term: ERL_NIF_TERM, head: *mut ERL_NIF_TERM, tail: *mut ERL_NIF_TERM",
    );
    b.func("c_int", "enif_get_tuple", "env: *mut ErlNifEnv, tpl: ERL_NIF_TERM, arity: *mut c_int, array: *mut *const ERL_NIF_TERM");
    b.func(
        "c_int",
        "enif_is_identical",
        "lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_compare",
        "lhs: ERL_NIF_TERM, rhs: ERL_NIF_TERM",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_binary",
        "env: *mut ErlNifEnv, bin: *mut ErlNifBinary",
    );
    b.func("ERL_NIF_TERM", "enif_make_badarg", "env: *mut ErlNifEnv");
    b.func(
        "ERL_NIF_TERM",
        "enif_make_int",
        "env: *mut ErlNifEnv, i: c_int",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_ulong",
        "env: *mut ErlNifEnv, i: c_ulong",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_double",
        "env: *mut ErlNifEnv, d: c_double",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_atom",
        "env: *mut ErlNifEnv, name: *const c_uchar",
    );
    b.func("c_int", "enif_make_existing_atom", "env: *mut ErlNifEnv, name: *const c_uchar, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding");

    b.variadic_func(
        "ERL_NIF_TERM",
        "enif_make_tuple",
        "env: *mut ErlNifEnv, cnt: c_uint",
    );
    b.variadic_func(
        "ERL_NIF_TERM",
        "enif_make_list",
        "env: *mut ErlNifEnv, cnt: c_uint",
    );

    b.func(
        "ERL_NIF_TERM",
        "enif_make_list_cell",
        "env: *mut ErlNifEnv, car: ERL_NIF_TERM, cdr: ERL_NIF_TERM",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_string",
        "env: *mut ErlNifEnv, string: *const c_uchar, arg1: ErlNifCharEncoding",
    );
    b.func("ERL_NIF_TERM", "enif_make_ref", "env: *mut ErlNifEnv");

    // Skip threading API for now (perhaps forever)
    // If anybody has a situation where they want to use this API instead of the very fine
    // Rust API, please tell me.
    //      Func("*mut ErlNifMutex", "enif_mutex_create", "name: *mut c_uchar"),
    //      Func("", "enif_mutex_destroy", "mtx: *mut ErlNifMutex"),
    //      Func("c_int", "enif_mutex_trylock", "mtx: *mut ErlNifMutex"),
    //      Func("", "enif_mutex_lock", "mtx: *mut ErlNifMutex"),
    //      Func("", "enif_mutex_unlock", "mtx: *mut ErlNifMutex"),
    //      Func("*mut ErlNifCond", "enif_cond_create", "name: *mut c_uchar"),
    //      Func("", "enif_cond_destroy", "cnd: *mut ErlNifCond"),
    //      Func("", "enif_cond_signal", "cnd: *mut ErlNifCond"),
    //      Func("", "enif_cond_broadcast", "cnd: *mut ErlNifCond"),
    //      Func("", "enif_cond_wait", "cnd: *mut ErlNifCond, mtx: *mut ErlNifMutex"),
    //      Func("*mut ErlNifRWLock", "enif_rwlock_create", "name: *mut c_uchar"),
    //      Func("", "enif_rwlock_destroy", "rwlck: *mut ErlNifRWLock"),
    //      Func("c_int", "enif_rwlock_tryrlock", "rwlck: *mut ErlNifRWLock"),
    //      Func("", "enif_rwlock_rlock", "rwlck: *mut ErlNifRWLock"),
    //      Func("", "enif_rwlock_runlock", "rwlck: *mut ErlNifRWLock"),
    //      Func("c_int", "enif_rwlock_tryrwlock", "rwlck: *mut ErlNifRWLock"),
    //      Func("", "enif_rwlock_rwlock", "rwlck: *mut ErlNifRWLock"),
    //      Func("", "enif_rwlock_rwunlock", "rwlck: *mut ErlNifRWLock"),
    //      Func("c_int", "enif_tsd_key_create", "name: *mut c_uchar, key: *mut ErlNifTSDKey"),
    //      Func("", "enif_tsd_key_destroy", "key: ErlNifTSDKey"),
    //      Func("", "enif_tsd_set", "key: ErlNifTSDKey, data: *mut c_void"),
    //      Func("*mut c_void", "enif_tsd_get", "key: ErlNifTSDKey"),
    //      Func("*mut ErlNifThreadOpts", "enif_thread_opts_create", "name: *mut c_uchar"),
    //      Func("", "enif_thread_opts_destroy", "opts: *mut ErlNifThreadOpts"),
    //      Func("c_int", "enif_thread_create", "name: *mut c_uchar, tid: *mut ErlNifTid, func: Option<unsafe extern \"C\" fn (arg1: *mut c_void) -> *mut c_void>, args: *mut c_void, opts: *mut ErlNifThreadOpts"),
    //      Func("ErlNifTid", "enif_thread_self", ""),
    //      Func("c_int", "enif_equal_tids", "tid1: ErlNifTid, tid2: ErlNifTid"),
    //      Func("", "enif_thread_exit", "resp: *mut c_void"),
    //      Func("c_int", "enif_thread_join", "arg1: ErlNifTid, respp: *mut *mut c_void"),
    b.dummy("dummy_enif_mutex_create");
    b.dummy("dummy_enif_mutex_destroy");
    b.dummy("dummy_enif_mutex_trylock");
    b.dummy("dummy_enif_mutex_lock");
    b.dummy("dummy_enif_mutex_unlock");
    b.dummy("dummy_enif_cond_create");
    b.dummy("dummy_enif_cond_destroy");
    b.dummy("dummy_enif_cond_signal");
    b.dummy("dummy_enif_cond_broadcast");
    b.dummy("dummy_enif_cond_wait");
    b.dummy("dummy_enif_rwlock_create");
    b.dummy("dummy_enif_rwlock_destroy");
    b.dummy("dummy_enif_rwlock_tryrlock");
    b.dummy("dummy_enif_rwlock_rlock");
    b.dummy("dummy_enif_rwlock_runlock");
    b.dummy("dummy_enif_rwlock_tryrwlock");
    b.dummy("dummy_enif_rwlock_rwlock");
    b.dummy("dummy_enif_rwlock_rwunlock");
    b.dummy("dummy_enif_tsd_key_create");
    b.dummy("dummy_enif_tsd_key_destroy");
    b.dummy("dummy_enif_tsd_set");
    b.dummy("dummy_enif_tsd_get");
    b.dummy("dummy_enif_thread_opts_create");
    b.dummy("dummy_enif_thread_opts_destroy");
    b.dummy("dummy_enif_thread_create");
    b.dummy("dummy_enif_thread_self");
    b.dummy("dummy_enif_equal_tids");
    b.dummy("dummy_enif_thread_exit");
    b.dummy("dummy_enif_thread_join");

    b.func(
        "*mut c_void",
        "enif_realloc",
        "ptr: *mut c_void, size: size_t",
    );
    b.func(
        "",
        "enif_system_info",
        "sip: *mut ErlNifSysInfo, si_size: size_t",
    );

    b.variadic_func(
        "c_int",
        "enif_fprintf",
        "filep: *mut c_void, format: *const c_uchar",
    );

    b.func(
        "c_int",
        "enif_inspect_iolist_as_binary",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_sub_binary",
        "arg1: *mut ErlNifEnv, bin_term: ERL_NIF_TERM, pos: size_t, size: size_t",
    );
    b.func("c_int", "enif_get_string", "arg1: *mut ErlNifEnv, list: ERL_NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding");
    b.func("c_int", "enif_get_atom", "arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, buf: *mut c_uchar, len: c_uint, arg2: ErlNifCharEncoding");
    b.func(
        "c_int",
        "enif_is_fun",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_is_pid",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_is_port",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_get_uint",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_uint",
    );
    b.func(
        "c_int",
        "enif_get_long",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_long",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_uint",
        "arg1: *mut ErlNifEnv, i: c_uint",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_long",
        "arg1: *mut ErlNifEnv, i: c_long",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_tuple_from_array",
        "arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_list_from_array",
        "arg1: *mut ErlNifEnv, arr: *const ERL_NIF_TERM, cnt: c_uint",
    );
    b.func(
        "c_int",
        "enif_is_empty_list",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func("*const ErlNifResourceType", "enif_open_resource_type", "arg1: *mut ErlNifEnv, module_str: *const c_char, name_str: *const c_char, dtor: Option<unsafe extern \"C\" fn (arg1: *mut ErlNifEnv, arg2: *mut c_void)>, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags");
    b.func(
        "*mut c_void",
        "enif_alloc_resource",
        "_type: *const ErlNifResourceType, size: size_t",
    );
    b.func("", "enif_release_resource", "obj: *const c_void");
    b.func(
        "ERL_NIF_TERM",
        "enif_make_resource",
        "arg1: *mut ErlNifEnv, obj: *const c_void",
    );
    b.func("c_int", "enif_get_resource", "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, _type: *const ErlNifResourceType, objp: *mut *const c_void");
    b.func("size_t", "enif_sizeof_resource", "obj: *const c_void");
    b.func(
        "*mut c_uchar",
        "enif_make_new_binary",
        "arg1: *mut ErlNifEnv, size: size_t, termp: *mut ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_is_list",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_is_tuple",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_get_atom_length",
        "arg1: *mut ErlNifEnv, atom: ERL_NIF_TERM, len: *mut c_uint, arg2: ErlNifCharEncoding",
    );
    b.func(
        "c_int",
        "enif_get_list_length",
        "env: *mut ErlNifEnv, term: ERL_NIF_TERM, len: *mut c_uint",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_atom_len",
        "env: *mut ErlNifEnv, name: *const c_char, len: size_t",
    );
    b.func("c_int", "enif_make_existing_atom_len", "env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, arg1: ErlNifCharEncoding");
    b.func(
        "ERL_NIF_TERM",
        "enif_make_string_len",
        "env: *mut ErlNifEnv, string: *const c_char, len: size_t, arg1: ErlNifCharEncoding",
    );
    b.func("*mut ErlNifEnv", "enif_alloc_env", "");
    b.func("", "enif_free_env", "env: *mut ErlNifEnv");
    b.func("", "enif_clear_env", "env: *mut ErlNifEnv");
    b.func(
        "c_int",
        "enif_send",
        "env: *mut ErlNifEnv, to_pid: *const ErlNifPid, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_make_copy",
        "dst_env: *mut ErlNifEnv, src_term: ERL_NIF_TERM",
    );
    b.func(
        "*mut ErlNifPid",
        "enif_self",
        "caller_env: *mut ErlNifEnv, pid: *mut ErlNifPid",
    );
    b.func(
        "c_int",
        "enif_get_local_pid",
        "env: *mut ErlNifEnv, arg1: ERL_NIF_TERM, pid: *mut ErlNifPid",
    );
    b.func("", "enif_keep_resource", "obj: *const c_void");
    b.func(
        "ERL_NIF_TERM",
        "enif_make_resource_binary",
        "arg1: *mut ErlNifEnv, obj: *const c_void, data: *const c_void, size: size_t",
    );

    if opts.ulong_size == 4 {
        b.func(
            "c_int",
            "enif_get_int64",
            "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_longlong",
        );
        b.func(
            "c_int",
            "enif_get_uint64",
            "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut c_ulonglong",
        );
        b.func(
            "ERL_NIF_TERM",
            "enif_make_int64",
            "env: *mut ErlNifEnv, i: c_longlong",
        );
        b.func(
            "ERL_NIF_TERM",
            "enif_make_uint64",
            "env: *mut ErlNifEnv, i: c_ulonglong",
        );
    }

    b.func(
        "c_int",
        "enif_is_exception",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_make_reverse_list",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM, list: *mut ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_is_number",
        "arg1: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func("*mut c_void", "enif_dlopen", "lib: *const c_char, err_handler: Option<unsafe extern \"C\" fn (arg1: *mut c_void, arg2: *const c_char)>, err_arg: *mut c_void");
    b.func("*mut c_void", "enif_dlsym", "handle: *mut c_void, symbol: *const c_char, err_handler: Option<unsafe extern \"C\" fn (arg1: *mut c_void, arg2: *const c_char)>, err_arg: *mut c_void");
    b.func(
        "c_int",
        "enif_consume_timeslice",
        "arg1: *mut ErlNifEnv, percent: c_int",
    );
    b.func(
        "c_int",
        "enif_is_map",
        "env: *mut ErlNifEnv, term: ERL_NIF_TERM",
    );
    b.func(
        "c_int",
        "enif_get_map_size",
        "env: *mut ErlNifEnv, term: ERL_NIF_TERM, size: *mut size_t",
    );
    b.func("ERL_NIF_TERM", "enif_make_new_map", "env: *mut ErlNifEnv");
    b.func("c_int", "enif_make_map_put", "env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM");
    b.func(
        "c_int",
        "enif_get_map_value",
        "env: *mut ErlNifEnv, map: ERL_NIF_TERM, key: ERL_NIF_TERM, value: *mut ERL_NIF_TERM",
    );
    b.func("c_int", "enif_make_map_update", "env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, value: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM");
    b.func(
        "c_int",
        "enif_make_map_remove",
        "env: *mut ErlNifEnv, map_in: ERL_NIF_TERM, key: ERL_NIF_TERM, map_out: *mut ERL_NIF_TERM",
    );
    b.func("c_int", "enif_map_iterator_create", "env: *mut ErlNifEnv, map: ERL_NIF_TERM, iter: *mut ErlNifMapIterator, entry: ErlNifMapIteratorEntry");
    b.func(
        "",
        "enif_map_iterator_destroy",
        "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator",
    );
    b.func(
        "c_int",
        "enif_map_iterator_is_head",
        "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator",
    );
    b.func(
        "c_int",
        "enif_map_iterator_is_tail",
        "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator",
    );
    b.func(
        "c_int",
        "enif_map_iterator_next",
        "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator",
    );
    b.func(
        "c_int",
        "enif_map_iterator_prev",
        "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator",
    );
    b.func("c_int", "enif_map_iterator_get_pair", "env: *mut ErlNifEnv, iter: *mut ErlNifMapIterator, key: *mut ERL_NIF_TERM, value: *mut ERL_NIF_TERM");
    b.func("ERL_NIF_TERM", "enif_schedule_nif", "env: *mut ErlNifEnv, fun_name: *const c_char, flags:c_int, fp: unsafe extern \"C\" fn(env: *mut ErlNifEnv, argc:c_int, argv:*const ERL_NIF_TERM) -> ERL_NIF_TERM, argc:c_int, argv:*const ERL_NIF_TERM");

    // exception
    b.func(
        "c_int",
        "enif_has_pending_exception",
        "env: *mut ErlNifEnv, reason: *mut ERL_NIF_TERM",
    );
    b.func(
        "ERL_NIF_TERM",
        "enif_raise_exception",
        "env: *mut ErlNifEnv, reason: ERL_NIF_TERM",
    );

    // getenv
    b.func(
        "c_int",
        "enif_getenv",
        "key: *const c_char, value: *mut c_char, value_size: *mut size_t",
    );

    // time
    b.func("ErlNifTime", "enif_monotonic_time", "unit: ErlNifTimeUnit");
    b.func("ErlNifTime", "enif_time_offset", "unit: ErlNifTimeUnit");
    b.func(
        "ErlNifTime",
        "enif_convert_time_unit",
        "time: ErlNifTime, from_unit: ErlNifTimeUnit, to_unit: ErlNifTimeUnit",
    );

    // for NIF version > or equal to 2.11
    if opts.nif_version >= (2, 11) {
        b.func("ERL_NIF_TERM", "enif_now_time", "env: *mut ErlNifEnv");
        b.func("ERL_NIF_TERM", "enif_cpu_time", "env: *mut ErlNifEnv");
        b.func(
            "ERL_NIF_TERM",
            "enif_make_unique_integer",
            "env: *mut ErlNifEnv, properties: ErlNifUniqueInteger",
        );
        b.func(
            "c_int",
            "enif_is_current_process_alive",
            "env: *mut ErlNifEnv",
        );
        b.func(
            "c_int",
            "enif_is_process_alive",
            "env: *mut ErlNifEnv, pid: *const ErlNifPid",
        );
        b.func(
            "c_int",
            "enif_is_port_alive",
            "env: *mut ErlNifEnv, port_id: *const ErlNifPort",
        );
        b.func(
            "c_int",
            "enif_get_local_port",
            "env: *mut ErlNifEnv, term: ERL_NIF_TERM, port_id: *mut ErlNifPort",
        );
        b.func(
            "c_int",
            "enif_term_to_binary",
            "env: *mut ErlNifEnv, term: ERL_NIF_TERM, bin: *mut ErlNifBinary",
        );
        b.func("usize", "enif_binary_to_term", "env: *mut ErlNifEnv, data: *const c_uchar, sz: usize, term: *mut ERL_NIF_TERM, opts: ErlNifBinaryToTerm");
        b.func("c_int", "enif_port_command", "env: *mut ErlNifEnv, to_port: *const ErlNifPort, msg_env: *mut ErlNifEnv, msg: ERL_NIF_TERM");
        b.func("c_int", "enif_thread_type", "");
        b.variadic_func(
            "c_int",
            "enif_snprintf",
            "out: *mut c_char, size: usize, format: *const c_char",
        );
    }

    if opts.nif_version >= (2, 12) {
        b.func("c_int",                     "enif_select",               "env: *mut ErlNifEnv, e: ErlNifEvent, flags: ErlNifSelectFlags, obj: *const c_void, pid: *const ErlNifPid, eref: ERL_NIF_TERM");
        b.func("*const ErlNifResourceType", "enif_open_resource_type_x", "env: *mut ErlNifEnv, name_str: *const c_char, init: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags");
        b.func("c_int",                     "enif_monitor_process",      "env: *mut ErlNifEnv, obj: *const c_void, pid: *const ErlNifPid, monitor: *mut ErlNifMonitor");
        b.func(
            "c_int",
            "enif_demonitor_process",
            "env: *mut ErlNifEnv, obj: *const c_void,  monitor: *const ErlNifMonitor",
        );
        b.func(
            "c_int",
            "enif_compare_monitors",
            "monitor1: *const ErlNifMonitor, monitor2: *const ErlNifMonitor",
        );
        b.func(
            "u64",
            "enif_hash",
            "hashtype: ErlNifHash, term: ERL_NIF_TERM, salt: u64",
        );
        b.func(
            "c_int",
            "enif_whereis_pid",
            "env: *mut ErlNifEnv, name: ERL_NIF_TERM, pid: *mut ErlNifPid",
        );
        b.func(
            "c_int",
            "enif_whereis_port",
            "env: *mut ErlNifEnv, name: ERL_NIF_TERM, port: *mut ErlNifPort",
        );
    }

    if opts.nif_version >= (2, 13) {
        // Skip iovec API for now (perhaps forever).
        // Consider safer Rust iovec crates like https://crates.io/crates/iovec instead of this API.
        // If anybody really does need this API in Rust, please file a bug.
        // Func("ErlNifIOQueue *",  "enif_ioq_create",     "ErlNifIOQueueOpts opts"),
        // Func("void",             "enif_ioq_destroy",    "ErlNifIOQueue *q"),
        // Func("int",              "enif_ioq_enq_binary", "ErlNifIOQueue *q, ErlNifBinary *bin, size_t skip"),
        // Func("int",              "enif_ioq_enqv",       "ErlNifIOQueue *q, ErlNifIOVec *iov, size_t skip"),
        // Func("size_t",           "enif_ioq_size",       "ErlNifIOQueue *q"),
        // Func("int",              "enif_ioq_deq",        "ErlNifIOQueue *q, size_t count, size_t *size"),
        // Func("SysIOVec*",        "enif_ioq_peek",       "ErlNifIOQueue *q, int *iovlen"),
        // Func("int",              "enif_inspect_iovec",  "ErlNifEnv *env, size_t max_length, ERL_NIF_TERM iovec_term, ERL_NIF_TERM *tail, ErlNifIOVec **iovec"),
        // Func("void",             "enif_free_iovec",     "ErlNifIOVec *iov")
        b.dummy("dummy_enif_ioq_create");
        b.dummy("dummy_enif_ioq_destroy");
        b.dummy("dummy_enif_ioq_enq_binary");
        b.dummy("dummy_enif_ioq_enqv");
        b.dummy("dummy_enif_ioq_size");
        b.dummy("dummy_enif_ioq_deq");
        b.dummy("dummy_enif_ioq_peek");
        b.dummy("dummy_enif_inspect_iovec");
        b.dummy("dummy_enif_free_iovec");
    }

    if opts.nif_version >= (2, 14) {
        // Skip iovec and synchronization APIs for now (perhaps forever).
        // Consider safer Rust iovec crates like https://crates.io/crates/iovec instead of this API.
        // If anybody really does need this API in Rust, please file a bug.
        // Func("int",  "enif_ioq_peek_head",        "ErlNifEnv *env, ErlNifIOQueue *q, size_t *size, ERL_NIF_TERM *head"),
        // Func("char*, "enif_mutex_name",           "ErlNifMutex*"),
        // Func("char*, "enif_cond_name",            "ErlNifCond*"),
        // Func("char*, "enif_rwlock_name",          "ErlNifRWLock*"),
        // Func("char*, "enif_thread_name",          "ErlNifTid"),
        b.dummy("dummy_enif_ioq_peek_head");
        b.dummy("dummy_enif_mutex_name");
        b.dummy("dummy_enif_cond_name");
        b.dummy("dummy_enif_rwlock_name");
        b.dummy("dummy_enif_thread_name");

        // See format! and write!
        // Func("int",  "enif_vfprintf",             "FILE*, const char *fmt, va_list"),
        // Func("int",  "enif_vsnprintf",            "char*, size_t, const char *fmt, va_list"),
        b.dummy("dummy_enif_vfprintf");
        b.dummy("dummy_enif_vsnprintf");

        b.func("c_int", "enif_make_map_from_arrays", "env: *mut ErlNifEnv, keys: *const ERL_NIF_TERM, values: *const ERL_NIF_TERM, cnt: usize, map_out: *mut ERL_NIF_TERM");
    }

    // 2.15 was introduced in OTP 22
    if opts.nif_version >= (2, 15) {
        b.func(
            "ErlNifTermType",
            "enif_term_type",
            "env: *mut ErlNifEnv, term: ERL_NIF_TERM",
        );

        b.func("c_int", "enif_is_pid_undefined", "pid: *const ErlNifPid");
        b.func("", "enif_set_pid_undefined", "pid: *mut ErlNifPid");
        b.func(
            "ERL_NIF_TERM",
            "enif_make_monitor_term",
            "env: *mut ErlNifEnv, mon: *const ErlNifMonitor",
        );
    }

    // 2.16 was introduced in OTP 24
    if opts.nif_version >= (2, 16) {
        b.func("*const ErlNifResourceType", "enif_init_resource_type", "env: *mut ErlNifEnv, name_str: *const c_char, init: *const ErlNifResourceTypeInit, flags: ErlNifResourceFlags, tried: *mut ErlNifResourceFlags");
        b.func("c_int", "enif_dynamic_resource_call", "env: *mut ErlNifEnv, module: ERL_NIF_TERM, name: ERL_NIF_TERM, rsrc: ERL_NIF_TERM, call_data: *const c_void");
    }

    // 2.17 was introduced in OTP 26
    if opts.nif_version >= (2, 17) {
        b.func(
            "c_int",
            "enif_set_option",
            "env: *mut ErlNifEnv, opt: ErlNifOption",
        );
        b.func("c_int", "enif_get_string_length", "env: *mut ErlNifEnv, list: ERL_NIF_TERM, len: *mut c_uint, encoding: ErlNifCharEncoding");
        b.func("c_int", "enif_make_new_atom", "env: *mut ErlNifEnv, name: *const c_char, atom: *mut ERL_NIF_TERM, encoding: ErlNifCharEncoding");
        b.func("c_int", "enif_make_new_atom_len", "env: *mut ErlNifEnv, name: *const c_char, len: size_t, atom: *mut ERL_NIF_TERM, encoding: ErlNifCharEncoding");
    }
}

fn get_nif_version_from_features() -> (u32, u32) {
    for major in ((MIN_SUPPORTED_VERSION.0)..=(MAX_SUPPORTED_VERSION.0)).rev() {
        for minor in ((MIN_SUPPORTED_VERSION.1)..=(MAX_SUPPORTED_VERSION.1)).rev() {
            if env::var(format!("CARGO_FEATURE_NIF_VERSION_{}_{}", major, minor)).is_ok() {
                return (major, minor);
            }
        }
    }
    panic!(
        "At least the minimal feature nif_version_{}_{} has to be defined",
        MIN_SUPPORTED_VERSION.0, MIN_SUPPORTED_VERSION.1
    );
}

fn main() {
    let nif_version = get_nif_version_from_features();
    let target_family_or_current =
        env::var("CARGO_CFG_TARGET_FAMILY").unwrap_or_else(|_| env::consts::FAMILY.to_string());

    let target_family = if target_family_or_current == "windows" {
        OsFamily::Win
    } else if cfg!(target_family = "unix") {
        OsFamily::Unix
    } else {
        panic!("Unsupported Operational System Family")
    };

    let target_pointer_width = match env::var("CARGO_CFG_TARGET_POINTER_WIDTH") {
       Ok(target_pointer_width) => target_pointer_width,
         Err(err) => panic!(
            "An error occurred while determining the pointer width to compile `rustler_sys` for:\n\n{:?}\n\nPlease report a bug.",
            err
        )
    };

    let ulong_size = match target_family {
        OsFamily::Win => 4,
        OsFamily::Unix => {
            if target_pointer_width == "32" {
                4
            } else if target_pointer_width == "64" {
                8
            } else {
                panic!("Unsupported target pointer width")
            }
        }
    };

    let opts = GenerateOptions {
        ulong_size,
        nif_version,
        target_family,
    };
    let api = generate(&opts);

    let out_dir = env::var("OUT_DIR")
        .map_err(|_| "Can't read OUT_DIR env variable.")
        .unwrap();

    let dest_path = Path::new(&out_dir).join(SNIPPET_NAME);
    fs::write(dest_path, api).unwrap();

    // The following lines are important to tell Cargo to recompile if something changes.
    println!("cargo:rerun-if-changed=build.rs");
}
