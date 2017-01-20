/*!
Low level Rust bindings to the [Erlang NIF API](http://www.erlang.org/doc/man/erl_nif.html).

# NIF Crate

A NIF module is built by creating a new crate that uses `erlang_nif-sys` as a dependency.
(more)

# NIF Functions

All NIF functions must have the following signature:

```
#[macro_use]
extern crate erlang_nif_sys;
use erlang_nif_sys::*;
# fn main(){} //0
fn my_nif(env: *mut ErlNifEnv,
          argc: c_int,
          args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    // ...
#   unsafe{enif_make_badarg(env)}
}

```

# NIF Module Initialization

## For the Impatient
```
#[macro_use]
extern crate erlang_nif_sys;
use erlang_nif_sys::*;

nif_init!("my_nif_module",[
        ("my_nif_fun1", 1, my_nif_fun1),
        ("my_dirty_fun2", 1, my_dirty_fun2, ERL_NIF_DIRTY_JOB_CPU_BOUND)
    ],
    {load: my_load}
);
# fn main(){} //1
# fn my_load(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int { 0 }
# fn my_nif_fun1(_: *mut ErlNifEnv,_: c_int,args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {unsafe{*args}}
# fn my_dirty_fun2(_: *mut ErlNifEnv,_: c_int,args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {unsafe{*args}}
```

## Details

The `erlang_nif-sys` analog of [`ERL_NIF_INIT()`](http://www.erlang.org/doc/man/erl_nif_init.html) is `nif_init!` which has the following form:

`nif_init!(module_name, [nif_funcs], {options})`

`module_name` must be a string literal, for example `"mynifmodule"`.


`nif_funcs` declares all the exported NIF functions for this module.  Each entry is declared as

`(name, arity, function, flags)`

`name` is a string literal indicating the name of the function as seen from Erlang code.
`arity` is an integer indicating how many parameter this function takes as seen from Erlang code.
`function` is the Rust implentation of the NIF and must be of the form
`Fn(env: *mut ErlNifEnv, argc: c_int, args: *const ERL_NIF_TERM) -> ERL_NIF_TERM`.  This is usually a plain
Rust function, but closures are permitted.
`flags` is optional and allows you to specify if this NIF is to run on a dirty scheduler.  See [dirty NIFs](http://www.erlang.org/doc/man/erl_nif.html#dirty_nifs)
in the Erlang docs.

The `options` are the NIF module intialization functions [`load`](http://www.erlang.org/doc/man/erl_nif.html#load), [`reload`](http://www.erlang.org/doc/man/erl_nif.html#reload),
[`upgrade`](http://www.erlang.org/doc/man/erl_nif.html#upgrade), and [`unload`](http://www.erlang.org/doc/man/erl_nif.html#unload).
Each is optional and is specified in struct-init style if present.  If no options are needed,
the curly braces may be elided.  Stub implementation of all these functions looks something like:

```
#[macro_use]
extern crate erlang_nif_sys;
use erlang_nif_sys::*;

nif_init!("mymod", [], {load: load, reload: reload, upgrade: upgrade, unload: unload});

fn load(env: *mut ErlNifEnv,
        priv_data: *mut *mut c_void,
        load_info: ERL_NIF_TERM)-> c_int { 0 }

fn reload(env: *mut ErlNifEnv,
          priv_data: *mut *mut c_void,
          load_info: ERL_NIF_TERM) -> c_int { 0 }

fn upgrade(env: *mut ErlNifEnv,
           priv_data: *mut *mut c_void,
           old_priv_data: *mut *mut c_void,
                      load_info: ERL_NIF_TERM) -> c_int { 0 }

fn unload(env: *mut ErlNifEnv,
          priv_data: *mut c_void) {}

# fn main(){} //2
```

# Invoking NIF API

As with any Rust FFI call, NIF API calls must be wrapped in `unsafe` blocks.
Below is an example of invoking NIF APIs along with an approach for dealing with
the the `args` parameter.

```
extern crate erlang_nif_sys;
use erlang_nif_sys::*;
use std::mem;
fn native_add(env: *mut ErlNifEnv,
              argc: c_int,
              args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
    unsafe {
        let mut a: c_int = mem::uninitialized();
        let mut b: c_int = mem::uninitialized();
        if argc == 2 &&
           0 != enif_get_int(env, *args, &mut a) &&
           0 != enif_get_int(env, *args.offset(1), &mut b) {
            enif_make_int(env, a+b)
         }
         else {
            enif_make_badarg(env)
         }
    }
}
# fn main(){} //3
```

*/

pub use std::os::raw::{c_int, c_void, c_uint, c_char, c_uchar, c_ulong, c_long, c_double};

#[allow(non_camel_case_types)]
pub type size_t = usize;

use std::option::Option;

#[allow(non_camel_case_types)]
pub type ERL_NIF_UINT = size_t;

#[allow(non_camel_case_types)]
pub type ERL_NIF_TERM = ERL_NIF_UINT;

//#[derive(Debug, Copy, Clone)]
//#[repr(C)]
//pub struct ERL_NIF_TERM(ERL_NIF_UINT);  // Don't do this, 32 bin calling convention is different for structs and ints.


/// See [ErlNifEnv](http://www.erlang.org/doc/man/erl_nif.html#ErlNifEnv) in the Erlang docs.
#[derive(Debug)]
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifEnv {dummy:c_int}

/// See [ErlNifFunc](http://www.erlang.org/doc/man/erl_nif.html#ErlNifFunc) in the Erlang docs.
// #[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifFunc {
    pub name:     *const u8,
    pub arity:    c_uint,
    pub function: unsafe extern "C" fn(env: *mut ErlNifEnv, argc: c_int, argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM,
    pub flags:    c_uint,
}

// #[allow(missing_copy_implementations)]
#[doc(hidden)]
#[derive(Debug)]
#[repr(C)]
pub struct ErlNifEntry {
    pub major:        c_int,
    pub minor:        c_int,
    pub name:         *const u8,
    pub num_of_funcs: c_int,
    pub funcs:        *const ErlNifFunc,
    pub load:    Option<unsafe extern "C" fn(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int>,
    pub reload:  Option<unsafe extern "C" fn(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
    pub upgrade: Option<unsafe extern "C" fn(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, old_priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
    pub unload:  Option<unsafe extern "C" fn(env: *mut ErlNifEnv, priv_data: *mut c_void) -> ()>,
    pub vm_variant: *const u8,
    pub options: c_uint,
}

pub const ERL_NIF_DIRTY_NIF_OPTION: c_uint = 1;

/// See [ErlNifBinary](http://www.erlang.org/doc/man/erl_nif.html#ErlNifBinary) in the Erlang docs.
#[allow(missing_copy_implementations)]
#[derive(Debug)]
#[repr(C)]
pub struct ErlNifBinary {
    pub size: size_t,
    pub data: *mut u8,
    bin_term: ERL_NIF_TERM,
    ref_bin: *mut c_void,
}

/// See [ErlNifResourceType](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceType) in the Erlang docs.
#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifResourceType {dummy:c_int}

/// See [ErlNifResourceDtor](http://www.erlang.org/doc/man/erl_nif.html#ErlNifResourceDtor) in the Erlang docs.
#[allow(missing_copy_implementations)]
pub type ErlNifResourceDtor = unsafe extern "C" fn(arg1: *mut ErlNifEnv, arg2: *mut c_void) -> ();

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
    DUMMY = 999, // prevents "univariant enum" compile error
}

/// See [ErlNifPid](http://www.erlang.org/doc/man/erl_nif.html#ErlNifPid) in the Erlang docs.
#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct ErlNifPid {
    pid: ERL_NIF_TERM,
}

/// See [enif_make_pid](http://erlang.org/doc/man/erl_nif.html#enif_make_pid) in the Erlang docs
pub unsafe fn enif_make_pid(_env: *mut ErlNifEnv, pid: & ErlNifPid) -> ERL_NIF_TERM {
    pid.pid
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
pub const ERL_NIF_TIME_ERROR: i64 = -9223372036854775808;
//const ERL_NIF_TIME_ERROR:i64 = i64::min_value();  "error: const fn's not yet stable"

/// See [ErlNifTimeUnit](http://www.erlang.org/doc/man/erl_nif.html#ErlNifTimeUnit) in the Erlang docs.
#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifTimeUnit {
    // values yanked from https://github.com/erlang/otp/blob/7cb403e4aa044fd2cc7702dbe8e2d0eea68e81f3/erts/emulator/beam/erl_drv_nif.h#L132
    ERL_NIF_SEC  = 0,
    ERL_NIF_MSEC = 1,
    ERL_NIF_USEC = 2,
    ERL_NIF_NSEC = 3,
}

/// See [ErlNifUniqueInteger](http://erlang.org/doc/man/erl_nif.html#ErlNifUniqueInteger) in the Erlang docs.
pub type ErlNifUniqueInteger = c_int;
pub const ERL_NIF_UNIQUE_POSITIVE: ErlNifUniqueInteger  = (1 << 0);
pub const ERL_NIF_UNIQUE_MONOTONIC: ErlNifUniqueInteger = (1 << 1);
// ref https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_nif.h#L203
// FIXME: Should actually be C enum, but repr(C) enums in Rust can't be used as bitfields.
//        Fix if the right abstraction ever lands in Rust.


/// See [ErlNifPort](http://erlang.org/doc/man/erl_nif.html#ErlNifPort) in the Erlang docs.
#[derive(Copy, Clone)]
#[repr(C)]
pub struct ErlNifPort {
    port_id: ERL_NIF_TERM,  // internal, may change
}
// ref https://github.com/erlang/otp/blob/maint/erts/emulator/beam/erl_nif.h#L155


/// See [ErlNifBinaryToTerm](http://erlang.org/doc/man/erl_nif.html#ErlNifBinaryToTerm) in the Erlang docs.
pub type ErlNifBinaryToTerm = c_int;
pub const ERL_NIF_BIN2TERM_SAFE: ErlNifBinaryToTerm = 0x20000000;


pub const ERL_NIF_THR_UNDEFINED: c_int =  0;
pub const ERL_NIF_THR_NORMAL_SCHEDULER: c_int =  1;
pub const ERL_NIF_THR_DIRTY_CPU_SCHEDULER: c_int =  2;
pub const ERL_NIF_THR_DIRTY_IO_SCHEDULER: c_int =  3;



include!(concat!(env!("OUT_DIR"), "/nif_api.snippet"));
// example of included content:
// extern "C" {
//     pub fn enif_priv_data(arg1: *mut ErlNifEnv) -> *mut c_void;
//     pub fn enif_alloc(size: size_t) -> *mut c_void;
//     pub fn enif_free(ptr: *mut c_void);
//     pub fn enif_is_atom(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
//     pub fn enif_is_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
// ...

/// Implement exported module init function needed by the Erlang runtime.
///
/// See [the module level documentation](index.html) for usage of `nif_init!`.
///
/// The pre-0.5.5 `nif_init!` format is deprecated but still supported.
/// An example of this format is ...
///
/// ```rust,ignore
/// nif_init!(b"my_nif_module\0", Some(load), None, None, None,
///     nif!(b"my_nif_fun1\0", 1, my_nif_fun1),
///     nif!(b"my_dirty_fun2\0", 1, my_dirty_fun2, ERL_NIF_DIRTY_JOB_CPU_BOUND)
/// );
/// ```
#[macro_export]
macro_rules! nif_init {
    ($($rest:tt)*) => (
        platform_nif_init!(
            get_entry!($($rest)*)
        );
    )
}

/// Platform specific NIF module initialization.
///
/// This macro is intended for higher level NIF libraries and not for direct
/// users of `erlang_nif-sys`.  See implementation of `nif_init!` for usage.
#[macro_export]
macro_rules! platform_nif_init {
    ($get_entry:expr) => (
        #[cfg(unix)]
        #[no_mangle]
        pub extern "C" fn nif_init() -> *const $crate::ErlNifEntry {
            $get_entry()
        }

        #[cfg(windows)]
        #[no_mangle]
        pub extern "C" fn nif_init(callbacks: *mut TWinDynNifCallbacks) -> *const $crate::ErlNifEntry {
            unsafe {
                WinDynNifCallbacks = Some(*callbacks);
            }
            //std::ptr::copy_nonoverlapping(callbacks, &WinDynNifCallbacks, std::mem::size_of<TWinDynNifCallbacks>());
            $get_entry()
        }
    )
}



/// Wrapper to deliver NIF args as Rust slice.
///
/// A macro wrapper that combines the argc and args parameters into a
/// more Rustic slice (`&[ERL_NIF_TERM]`).  On release builds this macro
/// incurs zero overhead.
///
///
/// # Examples
/// ```
/// #[macro_use]
/// extern crate erlang_nif_sys;
/// use erlang_nif_sys::*;
/// use std::mem;
///
/// nif_init!("mymod", [
///     ("native_add", 2, slice_args!(native_add))
/// ]);
///
/// fn native_add(env: *mut ErlNifEnv,
///               args: &[ERL_NIF_TERM]) -> ERL_NIF_TERM {
///     unsafe {
///         let mut a: c_int = mem::uninitialized();
///         let mut b: c_int = mem::uninitialized();
///         if args.len() == 2 &&
///            0 != enif_get_int(env, args[0], &mut a) &&
///            0 != enif_get_int(env, args[1], &mut b) {
///             enif_make_int(env, a+b)
///         }
///         else {
///            enif_make_badarg(env)
///         }
///     }
/// }
/// # fn main(){} //3
#[macro_export]
macro_rules! slice_args {
    ($f:expr) => (
        |env: *mut ErlNifEnv, argc: c_int, args: *const ERL_NIF_TERM| -> ERL_NIF_TERM {
            $f(env, std::slice::from_raw_parts(args, argc as usize))
        }
    );
}

/// Internal macro for implenting a ErlNifEntry-creating function.
#[doc(hidden)]
#[macro_export]
macro_rules! get_entry {
    // add default options if elided
    ( $module:expr, $funcs_tt:tt) => ( get_entry!($module, $funcs_tt, {}) );

    // strip trailing comma in funcs
    ( $module:expr, [$($funcs:tt),+,], $inits_tt:tt ) => ( get_entry!($module, [$($funcs),*], $inits_tt) );

    ( $module:expr, [$($funcs:tt),*], {$($inits:tt)*} ) => (
        || { // start closure
            const FUNCS: &'static [$crate::ErlNifFunc] = &[$(make_func_entry!($funcs)),*];

            // initialize as much as possible statically
            static mut ENTRY: $crate::ErlNifEntry = $crate::ErlNifEntry{
                major : $crate::NIF_MAJOR_VERSION,
                minor : $crate::NIF_MINOR_VERSION,
                name : concat!($module, "\0") as *const str as *const u8,
                num_of_funcs : 0 as $crate::c_int,
                funcs : &[] as *const $crate::ErlNifFunc,
                load: None,
                reload: None,
                upgrade: None,
                unload: None,
                vm_variant : b"beam.vanilla\0" as *const u8,
                options: $crate::ERL_NIF_ENTRY_OPTIONS,
            };

            // get a safe mutable reference once to avoid repeated unsafe
            let mut entry = unsafe { &mut ENTRY };

            // perform dynamic insertions
            entry.num_of_funcs = FUNCS.len() as $crate::c_int;
            entry.funcs = FUNCS.as_ptr();
            set_optionals!(entry, $($inits)*);
            entry // return static entry reference
        } // end closure
    );

    // For legacy nif_init!() invocation, deprecated
    ($module:expr, $load:expr, $reload:expr, $upgrade:expr, $unload:expr, $($func:expr),* ) => (
        || { // start closure
            const FUNCS: &'static [$crate::ErlNifFunc] = &[$($func),*];
            static mut ENTRY: $crate::ErlNifEntry = $crate::ErlNifEntry{
                major : $crate::NIF_MAJOR_VERSION,
                minor : $crate::NIF_MINOR_VERSION,
                name : $module as *const u8,
                num_of_funcs : 0 as $crate::c_int,
                funcs : &[] as *const $crate::ErlNifFunc,
                load :    $load,
                reload :  $reload,
                upgrade : $upgrade,
                unload :  $unload,
                vm_variant : b"beam.vanilla\0" as *const u8,
                options: $crate::ERL_NIF_ENTRY_OPTIONS,
            };
            // get a safe mutable reference once to avoid repeated unsafe
            let mut entry = unsafe { &mut ENTRY };

            // perform dynamic insertions
            entry.num_of_funcs = FUNCS.len() as $crate::c_int;
            entry.funcs = FUNCS.as_ptr();
            entry // return static entry reference
        } // end closure
    );
}


/// Create ErlNifFunc structure.  Use inside `nif_init!`. (Deprecated)
///
/// This is deprecated; see [the module level documentation](index.html)
/// for current `nif_init!` syntax.
#[macro_export]
macro_rules! nif{
    ($name:expr, $arity:expr, $function:expr, $flags:expr) => (
        $crate::ErlNifFunc { name:     $name as *const u8,
                             arity:    $arity,
                             function: $function,
                             flags:    $flags});

    ($name:expr, $arity:expr, $function:expr) => (
        nif!($name, $arity, $function, 0))
}


/// Internal macro to create an ErlNifEntry-creating function.
#[doc(hidden)]
#[macro_export]
macro_rules! make_func_entry {
    (($name:expr, $arity:expr, $function:expr, $flags:expr)) => (
        $crate::ErlNifFunc { name:     concat!($name, "\0") as *const str as *const u8,
                             arity:    $arity,
                             function: {
                                unsafe extern "C" fn wrapper(env: *mut ErlNifEnv, argc: c_int, args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
                                    $function(env, argc, args)
                                }
                                wrapper
                             },
                             flags:    $flags});

    (($name:expr, $arity:expr, $function:expr)) => (
        make_func_entry!(($name, $arity, $function, 0));
    );
}


/// Internal macro to deal with optional init functions.
#[doc(hidden)]
#[macro_export]
macro_rules! set_optionals {
    ($entry:ident, $fname:ident: $val:expr, $($rest:tt)*) => (
        set_optional!($entry, $fname, $val);
        set_optionals!($entry, $($rest)*);
    );
    ($entry:ident, $fname:ident: $val:expr) => (
        set_optional!($entry, $fname, $val);
    );
    //($entry:ident$($rest:tt)*) => ($($rest)*);
    ($entry:ident,) => ();
}

/// Internal macro to deal with optional init functions.
#[doc(hidden)]
#[macro_export]
macro_rules! set_optional {
    ($entry:ident, load, $val:expr)    => ( {
        unsafe extern "C" fn wrapper(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int {
            $val(env, priv_data, load_info)
        }
        $entry.load = Some(wrapper);
     });
    ($entry:ident, reload, $val:expr)  => ( {
        unsafe extern "C" fn wrapper(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int {
            $val(env, priv_data, load_info)
        }
        $entry.reload = Some(wrapper);
     });
    ($entry:ident, upgrade, $val:expr) => ( {
        unsafe extern "C" fn wrapper(env: *mut ErlNifEnv, priv_data: *mut *mut c_void, old_priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int {
            $val(env, priv_data, old_priv_data, load_info)
        }
        $entry.upgrade = Some(wrapper);
     });
    ($entry:ident, unload, $val:expr)  => ( {
        unsafe extern "C" fn wrapper(env: *mut ErlNifEnv, priv_data: *mut c_void) {
            $val(env, priv_data)
        }
        $entry.unload = Some(wrapper);
     });
}

#[cfg(test)]
mod tests {
    use super::*;
    use std;
    use std::ptr;
    use std::slice;
    use std::ffi::{CString, CStr};


    // Initializer tests


    fn load(_env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
        14
    }

    fn unload(_env: *mut ErlNifEnv, _priv_data: *mut c_void) {}

    fn raw_nif1(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
        (argc*7) as usize
    }

    fn raw_nif2(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
        (argc*11) as usize
    }

    fn slice_nif(_env: *mut ErlNifEnv, args: &[ERL_NIF_TERM]) -> ERL_NIF_TERM {
        args.len() * 17
    }

    extern "C" fn c_load(_env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
        114
    }

    extern "C" fn c_unload(_env: *mut ErlNifEnv, _priv_data: *mut c_void) {}

    extern "C" fn c_nif1(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
        (argc*19) as usize
    }

    unsafe fn unsafe_load(_env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
        15
    }

    unsafe fn unsafe_nif(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
        (argc*23) as usize
    }


    #[test]
    fn opt_empty() {
        let entry = get_entry!("empty", [])();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(None, entry.load);
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        assert_eq!(None, entry.unload);
    }

    #[test]
    fn opt_some1() {
        let entry = get_entry!("empty", [], {load: load})();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(14, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        assert_eq!(None, entry.unload);
    }

    #[test]
    fn opt_some2() {
        let entry = get_entry!("empty", [], {load: load, unload:unload})();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(14, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        unsafe{entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut())}; // shouldn't panic or crash
    }

    #[test]
    fn opt_some2b() {  // optionals in different order as opt_some2
        let entry = get_entry!("empty", [], {unload:unload, load: load})();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(14, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        unsafe{entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut())}; // shouldn't panic or crash
    }

    #[test]
    fn opt_closure() {  // optionals in different order as opt_some2
        let entry = get_entry!("empty", [], {load: |_,_,_|15})();
        assert_eq!(15, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
    }


    #[test]
    fn modname() {
        let entry = get_entry!("bananas", [])();
        assert_eq!(CString::new("bananas").unwrap().as_ref(), unsafe{CStr::from_ptr(entry.name as *const i8)} );
    }

    #[test]
    fn nif1() {
        let entry = get_entry!("nifs", [("raw1", 3, raw_nif1)])();
        let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
        assert_eq!(3,  funcs[0].arity);
        assert_eq!(28, unsafe{(funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())});
        assert_eq!(0,  funcs[0].flags);
    }

    #[test]
    fn nif2() {
        let entry = get_entry!("nifs", [("raw1", 3, raw_nif1),("raw2", 33, raw_nif2, ERL_NIF_DIRTY_JOB_IO_BOUND)])();
        let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
        assert_eq!(2, funcs.len());
        assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
        assert_eq!(3,  funcs[0].arity);
        assert_eq!(28, unsafe{(funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())});
        assert_eq!(0,  funcs[0].flags);
        assert_eq!(CString::new("raw2").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[1].name as *const i8)});
        assert_eq!(33,  funcs[1].arity);
        assert_eq!(44, unsafe{(funcs[1].function)(ptr::null_mut(), 4, ptr::null_mut())});
        assert_eq!(ERL_NIF_DIRTY_JOB_IO_BOUND,  funcs[1].flags);
    }

    #[test]
    fn nif_closure() {
        let entry = get_entry!("nifs", [("closure", 5, |_,argc,_| (argc*13) as usize )])();
        let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("closure").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
        assert_eq!(5,  funcs[0].arity);
        assert_eq!(52, unsafe{(funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())});
        assert_eq!(0,  funcs[0].flags);
    }

    #[test]
    fn nif_wrapped() {
        let entry = get_entry!("nifs", [("sliced", 6, slice_args!(slice_nif))])();
        let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("sliced").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
        assert_eq!(6,  funcs[0].arity);
        assert_eq!(34, unsafe{(funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())});
        assert_eq!(0,  funcs[0].flags);
    }

    #[test]
    fn legacy() {
        let entry = get_entry!(
            b"legacymod\0",
            Some(c_load), None, None, Some(c_unload),
            nif!(b"cnif_1\0", 7, c_nif1, ERL_NIF_DIRTY_JOB_IO_BOUND),
            nif!(b"cnif_2\0", 8, c_nif1))();
        let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};

        assert_eq!(CString::new("legacymod").unwrap().as_ref(), unsafe{CStr::from_ptr(entry.name as *const i8)} );

        assert_eq!(114, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        unsafe{entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut())}; // shouldn't panic or crash

        assert_eq!(2, funcs.len());

        assert_eq!(CString::new("cnif_1").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
        assert_eq!(7,  funcs[0].arity);
        assert_eq!(38, unsafe{(funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())});
        assert_eq!(ERL_NIF_DIRTY_JOB_IO_BOUND,  funcs[0].flags);

        assert_eq!(CString::new("cnif_2").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[1].name as *const i8)});
        assert_eq!(8,  funcs[1].arity);
        assert_eq!(57, unsafe{(funcs[1].function)(ptr::null_mut(), 3, ptr::null_mut())});
        assert_eq!(0,  funcs[1].flags);
    }

    #[test]
    fn trailing_comma() {
        get_entry!("nifs",
            [
                ("raw1", 3, raw_nif1),
                ("raw2", 33, raw_nif2, ERL_NIF_DIRTY_JOB_IO_BOUND),   // <- trailing comma
            ],
            {
                unload: unload,
                load: load,    // <- trailing comma
            })();

    }

    #[test]
    fn unsafe_callbacks() {
        let entry = get_entry!("unsafe_nifs",
            [
                ("unsafe_nif", 3, unsafe_nif)
            ],
            {
                load: unsafe_load
            })();
        let funcs = unsafe{slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize)};
        assert_eq!(15, unsafe{entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)});
        assert_eq!(CString::new("unsafe_nif").unwrap().as_ref(), unsafe{CStr::from_ptr(funcs[0].name as *const i8)});
        assert_eq!(3,  funcs[0].arity);
        assert_eq!(46, unsafe{(funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())});
        assert_eq!(0,  funcs[0].flags);

    }


}
