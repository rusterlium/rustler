/// Exports a given list of functions to a Erlang module.
///
/// This should be called exactly once in every NIF library. It will wrap and export the given rust
/// functions into the Erlang module.
///
/// The first argument is a string specifying what Erlang/Elixir module you want the function
/// exported into. In Erlang this will simply be the atom you named your module. In Elixir, all
/// modules are prefixed with `Elixir.<module path>`
///
/// The second argument is a list of 3-tuples. Each tuple contains information on a single exported
/// NIF function. The first tuple item is the name you want to export the function into, the second
/// is the arity (number of arguments) of the exported function. The third argument is a
/// indentifier of a rust function. This is where your actual NIF will be implemented.
///
/// The third argument is an `Option<fn(env: &Env, load_info: Term) -> bool>`. If this is
/// `Some`, the function will execute when the NIF is first loaded by the BEAM.
#[macro_export]
#[deprecated(since = "0.22.0", note = "Please use `rustler::init!` instead.")]
macro_rules! rustler_export_nifs {
    // Strip trailing comma.
    ($name:expr, [$( $exported_nif:tt ),+,], $on_load:expr) => {
        $crate::rustler_export_nifs!($name, [$( $exported_nif ),*], $on_load);
    };
    ($name:expr, [$( $exported_nif:tt ),*], $on_load:expr) => {
        static mut NIF_ENTRY: Option<$crate::codegen_runtime::DEF_NIF_ENTRY> = None;

        $crate::rustler_export_nifs!(internal_platform_init, ({
            // TODO: If an unwrap ever happens, we will unwind right into C! Fix this!

            extern "C" fn nif_load(
                env: $crate::codegen_runtime::NIF_ENV,
                _priv_data: *mut *mut $crate::codegen_runtime::c_void,
                load_info: $crate::codegen_runtime::NIF_TERM)
                -> $crate::codegen_runtime::c_int {
                unsafe {
                    $crate::codegen_runtime::handle_nif_init_call($on_load, env, load_info)
                }
            }

            const FUN_ENTRIES: &'static [$crate::codegen_runtime::DEF_NIF_FUNC] = &[
                $($crate::rustler_export_nifs!(internal_item_init, $exported_nif)),*
            ];

            let entry = $crate::codegen_runtime::DEF_NIF_ENTRY {
                major: $crate::codegen_runtime::NIF_MAJOR_VERSION,
                minor: $crate::codegen_runtime::NIF_MINOR_VERSION,
                name: concat!($name, "\x00") as *const str as *const u8,
                num_of_funcs: FUN_ENTRIES.len() as $crate::codegen_runtime::c_int,
                funcs: FUN_ENTRIES.as_ptr(),
                load: Some(nif_load),
                reload: None,
                upgrade: None,
                unload: None,
                vm_variant: b"beam.vanilla\x00".as_ptr(),
                options: 0,
                sizeof_ErlNifResourceTypeInit: $crate::codegen_runtime::get_nif_resource_type_init_size(),
            };
            unsafe { NIF_ENTRY = Some(entry) };

            unsafe { NIF_ENTRY.as_ref().unwrap() }
        }));
    };

    (internal_item_init, ($nif_name:expr, $nif_arity:expr, $nif_fun:path)) => {
        $crate::rustler_export_nifs!(internal_item_init, ($nif_name, $nif_arity, $nif_fun, $crate::schedule::SchedulerFlags::Normal))
    };
    (internal_item_init, ($nif_name:expr, $nif_arity:expr, $nif_fun:path, $nif_flag:expr)) => {
        $crate::codegen_runtime::DEF_NIF_FUNC {
            name: concat!($nif_name, "\x00") as *const str as *const u8,
            arity: $nif_arity,
            function: {
                extern "C" fn nif_func(
                    env: $crate::codegen_runtime::NIF_ENV,
                    argc: $crate::codegen_runtime::c_int,
                    argv: *const $crate::codegen_runtime::NIF_TERM)
                    -> $crate::codegen_runtime::NIF_TERM {
                        unsafe {
                            $crate::rustler_export_nifs!(
                                internal_handle_nif_call, ($nif_fun, $nif_arity, env, argc, argv))
                        }
                }
                nif_func
            },
            flags: ($nif_flag as $crate::schedule::SchedulerFlags) as u32,
        }
    };

    (internal_handle_nif_call, ($fun:path, $arity:expr, $env:expr, $argc:expr, $argv:expr)) => ({
        use $crate::Term;
        let env_lifetime = ();
        let env = $crate::Env::new(&env_lifetime, $env);

        let terms = ::std::slice::from_raw_parts($argv, $argc as usize)
            .iter()
            .map(|x| $crate::Term::new(env, *x))
            .collect::<Vec<Term>>();

        let result: ::std::thread::Result<_> = ::std::panic::catch_unwind(move || {
            $crate::codegen_runtime::NifReturnable::as_returned($fun(env, &terms), env)
        });

        match result {
            Ok(res) => res.apply(env),
            Err(_err) => $crate::codegen_runtime::raise_exception(
                env.as_c_arg(),
                $crate::types::atom::Atom::from_bytes(env, b"nif_panic").ok().unwrap().as_c_arg(),
            ),
        }
    });

    (internal_platform_init, ($inner:expr)) => {
        #[cfg(not(feature = "alternative_nif_init_name"))]
        #[cfg(unix)]
        #[no_mangle]
        pub extern "C" fn nif_init() -> *const $crate::codegen_runtime::DEF_NIF_ENTRY {
            $inner
        }

        #[cfg(not(feature = "alternative_nif_init_name"))]
        #[cfg(windows)]
        #[no_mangle]
        pub extern "C" fn nif_init(callbacks: *mut $crate::codegen_runtime::TWinDynNifCallbacks) -> *const $crate::codegen_runtime::DEF_NIF_ENTRY {
            unsafe {
                $crate::codegen_runtime::WIN_DYN_NIF_CALLBACKS = Some(*callbacks);
            }

            $inner
        }

        #[cfg(feature = "alternative_nif_init_name")]
        #[cfg(unix)]
        #[no_mangle]
        pub extern "C" fn rustler_nif_init() -> *const $crate::codegen_runtime::DEF_NIF_ENTRY {
            $inner
        }

        #[cfg(feature = "alternative_nif_init_name")]
        #[cfg(windows)]
        #[no_mangle]
        pub extern "C" fn rustler_nif_init(callbacks: *mut $crate::codegen_runtime::TWinDynNifCallbacks) -> *const $crate::codegen_runtime::DEF_NIF_ENTRY {
            unsafe {
                $crate::codegen_runtime::WIN_DYN_NIF_CALLBACKS = Some(*callbacks);
            }

            $inner
        }
    };
}
