use ::{NifEnv, NifTerm, NifError, c_int};
use ::atom::get_atom_init;
use ::ruster_export::{ErlNifEnv, ERL_NIF_TERM};
use std::panic::recover;
use ::wrapper::exception;

// This is the last level of rust safe rust code before the BEAM.
// No panics should go above this point, as they will unwrap into the C code and ruin the day.
pub fn handle_nif_call(function: for<'a> fn(&'a NifEnv, &Vec<NifTerm>) -> Result<NifTerm<'a>, NifError>, 
                       _arity: usize, 
                       r_env: *mut ErlNifEnv, 
                       argc: c_int, 
                       argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
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
