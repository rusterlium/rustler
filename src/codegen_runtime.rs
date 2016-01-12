use ::{NifEnv, NifTerm, NifError, c_int};
use ::atom::get_atom_init;
//use ::ruster_export::{ErlNifEnv, ERL_NIF_TERM};
use ::wrapper::nif_interface::{NIF_RESOURCE_HANDLE, NIF_ENV, NIF_TERM};
use std::panic::recover;
use ::wrapper::exception;
use ::resource::NifResourceStruct;

// This is the last level of rust safe rust code before the BEAM.
// No panics should go above this point, as they will unwrap into the C code and ruin the day.
pub fn handle_nif_call(function: for<'a> fn(&'a NifEnv, &Vec<NifTerm>) -> Result<NifTerm<'a>, NifError>, 
                       _arity: usize, 
                       r_env: NIF_ENV, 
                       argc: c_int, 
                       argv: *const NIF_TERM) -> NIF_TERM {
    let env = NifEnv { env: r_env };
    
    let terms = unsafe { ::std::slice::from_raw_parts(argv, argc as usize) }.iter()
        .map(|x| NifTerm::new(&env, *x)).collect::<Vec<NifTerm>>();

    let result: ::std::thread::Result<Result<NifTerm, NifError>> = recover(|| function(&env, &terms));

    match result {
        Ok(res) => match res {
            Ok(ret) => ret.as_c_arg(),
            Err(err) => err.to_term(&env).as_c_arg(),
        },
        Err(_err) => {
            exception::throw(env.as_c_arg(), get_atom_init("nif_panic").to_term(&env).as_c_arg())
        },
    }
}

pub fn handle_nif_init_call(function: Option<for<'a> fn(&'a NifEnv, NifTerm) -> bool>,
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


use std::sync::RwLock;
use std;
use ::resource::align_alloced_mem_for_struct;
pub unsafe fn handle_drop_resource_struct_handle<T: NifResourceStruct>(env: NIF_ENV, handle: NIF_RESOURCE_HANDLE) {
    let aligned = align_alloced_mem_for_struct::<RwLock<T>>(handle);
    let res = aligned as *mut RwLock<T>;
    std::mem::drop(std::ptr::read(res));
}
