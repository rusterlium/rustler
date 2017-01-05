//! Functions used by runtime generated code. Should not be used.

use ::{NifEnv, NifTerm};
use ::types::atom::get_atom_init;
use ::wrapper::nif_interface::{MUTABLE_NIF_RESOURCE_HANDLE, NIF_ENV, NIF_TERM};
use std::panic::catch_unwind;
use ::wrapper::exception;
use ::resource::NifResourceTypeProvider;
use ::NifResult;

// Exports for runtime
pub use ::wrapper::nif_interface::{c_int, c_void};

// This is the last level of rust safe rust code before the BEAM.
// No panics should go above this point, as they will unwrap into the C code and ruin the day.
pub fn handle_nif_call(function: for<'a> fn(&'a NifEnv, &Vec<NifTerm<'a>>) -> NifResult<NifTerm<'a>>,
                       _arity: usize, r_env: NIF_ENV,
                       argc: c_int, argv: *const NIF_TERM) -> NIF_TERM {
    let env = NifEnv { env: r_env };

    let terms = unsafe { ::std::slice::from_raw_parts(argv, argc as usize) }.iter()
        .map(|x| NifTerm::new(&env, *x)).collect::<Vec<NifTerm>>();

    let result: ::std::thread::Result<NIF_TERM> = catch_unwind(|| {
        match function(&env, &terms) {
            Ok(ret) => ret.as_c_arg(),
            Err(err) => unsafe { err.encode(&env) }.as_c_arg(),
        }
    });

    match result {
        Ok(res) => res,
        Err(_err) => {
            exception::raise_exception(
                env.as_c_arg(),
                get_atom_init("nif_panic").to_term(&env).as_c_arg())
        },
    }
}

pub fn handle_nif_init_call(function: Option<for<'a> fn(&'a NifEnv, NifTerm<'a>) -> bool>,
                            r_env: NIF_ENV,
                            load_info: NIF_TERM) -> c_int {
    let env = NifEnv { env: r_env };
    let term = NifTerm::new(&env, load_info);

    if let Some(inner) = function {
        if inner(&env, term) { 0 } else { 1 }
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
