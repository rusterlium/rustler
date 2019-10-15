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
    ($get_entry:expr) => {
        #[cfg(unix)]
        #[no_mangle]
        pub extern "C" fn nif_init() -> *const $crate::rustler_sys_api::ErlNifEntry {
            $get_entry()
        }

        #[cfg(windows)]
        #[no_mangle]
        pub extern "C" fn nif_init(
            callbacks: *mut $crate::rustler_sys_api::TWinDynNifCallbacks,
        ) -> *const $crate::rustler_sys_api::ErlNifEntry {
            unsafe {
                WIN_DYN_NIF_CALLBACKS = Some(*callbacks);
            }
            //std::ptr::copy_nonoverlapping(callbacks, &WinDynNifCallbacks, std::mem::size_of<TWinDynNifCallbacks>());
            $get_entry()
        }
    };
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
/// extern crate rustler_sys;
/// use rustler_sys::*;
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
    ($f:expr) => {{
        use $crate::rustler_sys_api as ens;
        |env: *mut ens::ErlNifEnv,
         argc: ens::c_int,
         args: *const ens::ERL_NIF_TERM|
         -> ens::ERL_NIF_TERM { $f(env, std::slice::from_raw_parts(args, argc as usize)) }
    }};
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
            use $crate::rustler_sys_api as ens;
            const FUNCS: &'static [ens::ErlNifFunc] = &[$(make_func_entry!($funcs)),*];

            // initialize as much as possible statically
            static mut ENTRY: ens::ErlNifEntry = ens::ErlNifEntry{
                major : ens::NIF_MAJOR_VERSION,
                minor : ens::NIF_MINOR_VERSION,
                name : concat!($module, "\0") as *const str as *const u8,
                num_of_funcs : 0 as ens::c_int,
                funcs : &[] as *const ens::ErlNifFunc,
                load: None,
                reload: None,
                upgrade: None,
                unload: None,
                vm_variant : b"beam.vanilla\0" as *const u8,
                options: ens::ERL_NIF_ENTRY_OPTIONS,
                sizeof_ErlNifResourceTypeInit: 0,
            };

            // get a safe mutable reference once to avoid repeated unsafe
            let entry = unsafe { &mut ENTRY };

            // perform dynamic insertions
            entry.num_of_funcs = FUNCS.len() as ens::c_int;
            entry.funcs = FUNCS.as_ptr();
            set_optionals!(entry, $($inits)*);
            entry.sizeof_ErlNifResourceTypeInit = std::mem::size_of::<ens::ErlNifResourceTypeInit>();
            entry // return static entry reference
        } // end closure
    );

    // For legacy nif_init!() invocation, deprecated
    ($module:expr, $load:expr, $reload:expr, $upgrade:expr, $unload:expr, $($func:expr),* ) => (
        || { // start closure
            use $crate::rustler_sys_api as ens;
            const FUNCS: &'static [ens::ErlNifFunc] = &[$($func),*];
            static mut ENTRY: ens::ErlNifEntry = ens::ErlNifEntry{
                major : ens::NIF_MAJOR_VERSION,
                minor : ens::NIF_MINOR_VERSION,
                name : $module as *const u8,
                num_of_funcs : 0 as ens::c_int,
                funcs : &[] as *const ens::ErlNifFunc,
                load :    $load,
                reload :  $reload,
                upgrade : $upgrade,
                unload :  $unload,
                vm_variant : b"beam.vanilla\0" as *const u8,
                options: ens::ERL_NIF_ENTRY_OPTIONS,
                sizeof_ErlNifResourceTypeInit: 0,
            };
            // get a safe mutable reference once to avoid repeated unsafe
            let entry = unsafe { &mut ENTRY };

            // perform dynamic insertions
            entry.num_of_funcs = FUNCS.len() as ens::c_int;
            entry.funcs = FUNCS.as_ptr();
            entry.sizeof_ErlNifResourceTypeInit = std::mem::size_of::<ens::ErlNifResourceTypeInit>();
            entry // return static entry reference
        } // end closure
    );
}

/// Create ErlNifFunc structure.  Use inside `nif_init!`. (Deprecated)
///
/// This is deprecated; see [the module level documentation](index.html)
/// for current `nif_init!` syntax.
#[macro_export]
macro_rules! nif {
    ($name:expr, $arity:expr, $function:expr, $flags:expr) => {
        ens::ErlNifFunc {
            name: $name as *const u8,
            arity: $arity,
            function: $function,
            flags: $flags,
        }
    };

    ($name:expr, $arity:expr, $function:expr) => {
        nif!($name, $arity, $function, 0)
    };
}

/// Internal macro to create an ErlNifEntry-creating function.
#[doc(hidden)]
#[macro_export]
macro_rules! make_func_entry {
    (($name:expr, $arity:expr, $function:expr, $flags:expr)) => {
        ens::ErlNifFunc {
            name: concat!($name, "\0") as *const str as *const u8,
            arity: $arity,
            function: {
                unsafe extern "C" fn wrapper(
                    env: *mut ens::ErlNifEnv,
                    argc: ens::c_int,
                    args: *const ens::ERL_NIF_TERM,
                ) -> ens::ERL_NIF_TERM {
                    $function(env, argc, args)
                }
                wrapper
            },
            flags: $flags,
        }
    };

    (($name:expr, $arity:expr, $function:expr)) => {
        make_func_entry!(($name, $arity, $function, 0));
    };
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
    ($entry:ident, load, $val:expr) => {{
        unsafe extern "C" fn wrapper(
            env: *mut ens::ErlNifEnv,
            priv_data: *mut *mut ens::c_void,
            load_info: ens::ERL_NIF_TERM,
        ) -> ens::c_int {
            $val(env, priv_data, load_info)
        }
        $entry.load = Some(wrapper);
    }};
    ($entry:ident, reload, $val:expr) => {{
        unsafe extern "C" fn wrapper(
            env: *mut ens::ErlNifEnv,
            priv_data: *mut *mut ens::c_void,
            load_info: ens::ERL_NIF_TERM,
        ) -> ens::c_int {
            $val(env, priv_data, load_info)
        }
        $entry.reload = Some(wrapper);
    }};
    ($entry:ident, upgrade, $val:expr) => {{
        unsafe extern "C" fn wrapper(
            env: *mut ens::ErlNifEnv,
            priv_data: *mut *mut ens::c_void,
            old_priv_data: *mut *mut ens::c_void,
            load_info: ens::ERL_NIF_TERM,
        ) -> ens::c_int {
            $val(env, priv_data, old_priv_data, load_info)
        }
        $entry.upgrade = Some(wrapper);
    }};
    ($entry:ident, unload, $val:expr) => {{
        unsafe extern "C" fn wrapper(env: *mut ens::ErlNifEnv, priv_data: *mut ens::c_void) {
            $val(env, priv_data)
        }
        $entry.unload = Some(wrapper);
    }};
}

#[cfg(test)]
mod initmacro_namespace_tests {

    // explicitly disable for this test:
    // use rustler_sys_api::*;
    use crate::rustler_sys_api;

    use std;
    use std::ffi::{CStr, CString};
    use std::ptr;
    use std::slice;

    // Initializer tests
    fn load(
        _env: *mut rustler_sys_api::ErlNifEnv,
        _priv_data: *mut *mut rustler_sys_api::c_void,
        _load_info: rustler_sys_api::ERL_NIF_TERM,
    ) -> rustler_sys_api::c_int {
        14
    }

    fn unload(_env: *mut rustler_sys_api::ErlNifEnv, _priv_data: *mut rustler_sys_api::c_void) {}

    fn raw_nif1(
        _env: *mut rustler_sys_api::ErlNifEnv,
        argc: rustler_sys_api::c_int,
        _args: *const rustler_sys_api::ERL_NIF_TERM,
    ) -> rustler_sys_api::ERL_NIF_TERM {
        (argc * 7) as usize
    }

    fn slice_nif(
        _env: *mut rustler_sys_api::ErlNifEnv,
        args: &[rustler_sys_api::ERL_NIF_TERM],
    ) -> rustler_sys_api::ERL_NIF_TERM {
        args.len() * 17
    }

    #[test]
    fn opt_some2() {
        let entry = get_entry!("empty", [], {load: load, unload:unload})();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(14, unsafe {
            entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)
        });
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        unsafe { entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut()) }; // shouldn't panic or crash
    }

    #[test]
    fn nif1() {
        let entry = get_entry!("nifs", [("raw1", 3, raw_nif1)])();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(3, funcs[0].arity);
        assert_eq!(28, unsafe {
            (funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())
        });
        assert_eq!(0, funcs[0].flags);
    }

    #[test]
    fn nif_wrapped() {
        let entry = get_entry!("nifs", [("sliced", 6, slice_args!(slice_nif))])();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("sliced").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(6, funcs[0].arity);
        assert_eq!(34, unsafe {
            (funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())
        });
        assert_eq!(0, funcs[0].flags);
    }
}

#[cfg(test)]
mod initmacro_tests {
    use crate::rustler_sys_api::*;
    use std;
    use std::ffi::{CStr, CString};
    use std::ptr;
    use std::slice;

    // Initializer tests

    fn load(_env: *mut ErlNifEnv, _priv_data: *mut *mut c_void, _load_info: ERL_NIF_TERM) -> c_int {
        14
    }

    fn unload(_env: *mut ErlNifEnv, _priv_data: *mut c_void) {}

    fn raw_nif1(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
        (argc * 7) as usize
    }

    fn raw_nif2(_env: *mut ErlNifEnv, argc: c_int, _args: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
        (argc * 11) as usize
    }

    fn slice_nif(_env: *mut ErlNifEnv, args: &[ERL_NIF_TERM]) -> ERL_NIF_TERM {
        args.len() * 17
    }

    extern "C" fn c_load(
        _env: *mut ErlNifEnv,
        _priv_data: *mut *mut c_void,
        _load_info: ERL_NIF_TERM,
    ) -> c_int {
        114
    }

    extern "C" fn c_unload(_env: *mut ErlNifEnv, _priv_data: *mut c_void) {}

    extern "C" fn c_nif1(
        _env: *mut ErlNifEnv,
        argc: c_int,
        _args: *const ERL_NIF_TERM,
    ) -> ERL_NIF_TERM {
        (argc * 19) as usize
    }

    unsafe fn unsafe_load(
        _env: *mut ErlNifEnv,
        _priv_data: *mut *mut c_void,
        _load_info: ERL_NIF_TERM,
    ) -> c_int {
        15
    }

    unsafe fn unsafe_nif(
        _env: *mut ErlNifEnv,
        argc: c_int,
        _args: *const ERL_NIF_TERM,
    ) -> ERL_NIF_TERM {
        (argc * 23) as usize
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
        let entry = get_entry!("empty", [], { load: load })();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(14, unsafe {
            entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)
        });
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        assert_eq!(None, entry.unload);
    }

    #[test]
    fn opt_some2() {
        let entry = get_entry!("empty", [], {load: load, unload:unload})();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(14, unsafe {
            entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)
        });
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        unsafe { entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut()) }; // shouldn't panic or crash
    }

    #[test]
    fn opt_some2b() {
        // optionals in different order as opt_some2
        let entry = get_entry!("empty", [], {unload:unload, load: load})();
        assert_eq!(0, entry.num_of_funcs);
        assert_eq!(14, unsafe {
            entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)
        });
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        unsafe { entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut()) }; // shouldn't panic or crash
    }

    #[test]
    fn opt_closure() {
        // optionals in different order as opt_some2
        let entry = get_entry!("empty", [], {load: |_,_,_|15})();
        assert_eq!(15, unsafe {
            entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)
        });
    }

    #[test]
    fn modname() {
        let entry = get_entry!("bananas", [])();
        assert_eq!(CString::new("bananas").unwrap().as_ref(), unsafe {
            CStr::from_ptr(entry.name as *const i8)
        });
    }

    #[test]
    fn nif1() {
        let entry = get_entry!("nifs", [("raw1", 3, raw_nif1)])();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(3, funcs[0].arity);
        assert_eq!(28, unsafe {
            (funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())
        });
        assert_eq!(0, funcs[0].flags);
    }

    #[test]
    fn nif2() {
        let entry = get_entry!(
            "nifs",
            [
                ("raw1", 3, raw_nif1),
                ("raw2", 33, raw_nif2, ERL_NIF_DIRTY_JOB_IO_BOUND)
            ]
        )();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        assert_eq!(2, funcs.len());
        assert_eq!(CString::new("raw1").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(3, funcs[0].arity);
        assert_eq!(28, unsafe {
            (funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())
        });
        assert_eq!(0, funcs[0].flags);
        assert_eq!(CString::new("raw2").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[1].name as *const i8)
        });
        assert_eq!(33, funcs[1].arity);
        assert_eq!(44, unsafe {
            (funcs[1].function)(ptr::null_mut(), 4, ptr::null_mut())
        });
        assert_eq!(ERL_NIF_DIRTY_JOB_IO_BOUND, funcs[1].flags);
    }

    #[test]
    fn nif_closure() {
        let entry = get_entry!("nifs", [("closure", 5, |_, argc, _| (argc * 13) as usize)])();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("closure").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(5, funcs[0].arity);
        assert_eq!(52, unsafe {
            (funcs[0].function)(ptr::null_mut(), 4, ptr::null_mut())
        });
        assert_eq!(0, funcs[0].flags);
    }

    #[test]
    fn nif_wrapped() {
        let entry = get_entry!("nifs", [("sliced", 6, slice_args!(slice_nif))])();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        assert_eq!(1, funcs.len());
        assert_eq!(CString::new("sliced").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(6, funcs[0].arity);
        assert_eq!(34, unsafe {
            (funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())
        });
        assert_eq!(0, funcs[0].flags);
    }

    #[test]
    fn legacy() {
        let entry = get_entry!(
            b"legacymod\0",
            Some(c_load),
            None,
            None,
            Some(c_unload),
            nif!(b"cnif_1\0", 7, c_nif1, ERL_NIF_DIRTY_JOB_IO_BOUND),
            nif!(b"cnif_2\0", 8, c_nif1)
        )();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };

        assert_eq!(CString::new("legacymod").unwrap().as_ref(), unsafe {
            CStr::from_ptr(entry.name as *const i8)
        });

        assert_eq!(114, unsafe {
            entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)
        });
        assert_eq!(None, entry.reload);
        assert_eq!(None, entry.upgrade);
        unsafe { entry.unload.unwrap()(ptr::null_mut(), ptr::null_mut()) }; // shouldn't panic or crash

        assert_eq!(2, funcs.len());

        assert_eq!(CString::new("cnif_1").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(7, funcs[0].arity);
        assert_eq!(38, unsafe {
            (funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())
        });
        assert_eq!(ERL_NIF_DIRTY_JOB_IO_BOUND, funcs[0].flags);

        assert_eq!(CString::new("cnif_2").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[1].name as *const i8)
        });
        assert_eq!(8, funcs[1].arity);
        assert_eq!(57, unsafe {
            (funcs[1].function)(ptr::null_mut(), 3, ptr::null_mut())
        });
        assert_eq!(0, funcs[1].flags);
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
        let entry = get_entry!("unsafe_nifs", [("unsafe_nif", 3, unsafe_nif)], {
            load: unsafe_load
        })();
        let funcs = unsafe { slice::from_raw_parts(entry.funcs, entry.num_of_funcs as usize) };
        assert_eq!(15, unsafe {
            entry.load.unwrap()(ptr::null_mut(), ptr::null_mut(), 0)
        });
        assert_eq!(CString::new("unsafe_nif").unwrap().as_ref(), unsafe {
            CStr::from_ptr(funcs[0].name as *const i8)
        });
        assert_eq!(3, funcs[0].arity);
        assert_eq!(46, unsafe {
            (funcs[0].function)(ptr::null_mut(), 2, ptr::null_mut())
        });
        assert_eq!(0, funcs[0].flags);
    }
}
