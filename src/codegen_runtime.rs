//! Functions used by runtime generated code. Should not be used.

use ::{NifEnv, NifTerm};
use ::types::atom::NifAtom;
use ::wrapper::exception;
use ::resource::NifResourceTypeProvider;
use ::NifResult;
use std::cell::Cell;
use std::panic::catch_unwind;
use std::ptr;

// Names used by the `rustler_export_nifs!` macro or other generated code.
pub use ::wrapper::nif_interface::{
    c_int, c_void, DEF_NIF_ENTRY, DEF_NIF_FUNC,
    NIF_ENV, NIF_TERM, NIF_MAJOR_VERSION, NIF_MINOR_VERSION,
    MUTABLE_NIF_RESOURCE_HANDLE };

thread_local! {
    /// The NIF_ENV for the current NIF call on this thread, if any. Otherwise null. This is never
    /// set to an `OwnedEnv`.
    ///
    /// With use this to obey the documented contract for `enif_send`. See the implementation of
    /// `OwnedEnv::send()`.
    static CALLING_PROCESS_ENV: Cell<NIF_ENV> = Cell::new(ptr::null_mut());
}

/// The NIF_ENV for the current NIF call on this thread, if any. Otherwise null.
pub fn get_calling_process_env() -> NIF_ENV {
    CALLING_PROCESS_ENV.with(|current| current.get())
}

/// A value that does something when it's dropped.
struct OnDrop<F: FnMut()>(F);

impl<F: FnMut()> Drop for OnDrop<F> {
    fn drop(&mut self) { self.0(); }
}

// This is the last level of rust safe rust code before the BEAM.
// No panics should go above this point, as they will unwrap into the C code and ruin the day.
pub fn handle_nif_call(function: for<'a> fn(NifEnv<'a>, &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>>,
                       _arity: usize, r_env: NIF_ENV,
                       argc: c_int, argv: *const NIF_TERM) -> NIF_TERM {
    CALLING_PROCESS_ENV.with(|current| current.set(r_env));
    let _reset_calling_process = OnDrop(|| {
        CALLING_PROCESS_ENV.with(|current| current.set(ptr::null_mut()))
    });

    let env_lifetime = ();
    let env = unsafe { NifEnv::new(&env_lifetime, r_env) };

    let terms = unsafe { ::std::slice::from_raw_parts(argv, argc as usize) }.iter()
        .map(|x| NifTerm::new(env, *x)).collect::<Vec<NifTerm>>();

    let result: ::std::thread::Result<NIF_TERM> = catch_unwind(|| {
        match function(env, &terms) {
            Ok(ret) => ret.as_c_arg(),
            Err(err) => unsafe { err.encode(env) }.as_c_arg(),
        }
    });

    match result {
        Ok(res) => res,
        Err(_err) => unsafe {
            exception::raise_exception(
                env.as_c_arg(),
                NifAtom::from_bytes(env, b"nif_panic").ok().unwrap().as_c_arg())
        },
    }
}

pub fn handle_nif_init_call(function: Option<for<'a> fn(NifEnv<'a>, NifTerm<'a>) -> bool>,
                            r_env: NIF_ENV,
                            load_info: NIF_TERM) -> c_int {
    let env_lifetime = ();
    let env = unsafe { NifEnv::new(&env_lifetime, r_env) };
    let term = NifTerm::new(env, load_info);

    if let Some(inner) = function {
        if inner(env, term) { 0 } else { 1 }
    } else {
        0
    }
}


use std;
use ::resource::align_alloced_mem_for_struct;
pub unsafe fn handle_drop_resource_struct_handle<T: NifResourceTypeProvider>(_env: NIF_ENV, handle: MUTABLE_NIF_RESOURCE_HANDLE) {
    let aligned = align_alloced_mem_for_struct::<Box<T>>(handle);
    let res = aligned as *mut Box<T>;
    std::mem::drop(std::ptr::read(res));
}
