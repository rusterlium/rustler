#![feature(recover, std_panic)]

mod wrapper;

#[macro_use]
extern crate lazy_static;

extern crate ruster_unsafe;
pub use self::ruster_unsafe::{ ERL_NIF_TERM, ErlNifResourceFlags, ErlNifResourceType };

pub mod ruster_export {
    pub use super::ruster_unsafe::*;
}

extern crate libc;
pub use libc::{ c_char, size_t, c_int, c_uint, c_void };

use std::mem;
use std::marker::PhantomData;

use std::any;
use std::panic;

mod types;
pub use self::types::{ NifEncoder, NifDecoder };

mod resource;
pub use self::resource::{ open_resource_type_raw, alloc_resource_raw };
pub use self::resource::{ open_struct_resource_type, alloc_struct_resource, get_struct_resource };

mod binary;
pub use self::binary::{ NifBinary, alloc_binary, make_binary, get_binary };

#[macro_reexport]
pub mod tuple;

pub mod atom;
pub use self::atom::{ init_atom, get_atom, get_atom_init };

pub struct NifEnv {
    pub env: *mut ruster_unsafe::ErlNifEnv,
}
impl NifEnv {
    pub fn as_c_arg(&self) -> *mut ruster_unsafe::ErlNifEnv {
        self.env
    }
}

#[derive(Clone, Copy)]
pub enum NifError {
    BadArg,
    AllocFail,
    Atom(&'static str),
}
impl NifError {
    pub fn to_term<'a>(self, env: &'a NifEnv) -> NifTerm<'a> {
        NifTerm::new(env, match self {
            NifError::BadArg => 
                unsafe { ruster_export::enif_make_badarg(env.as_c_arg()) },
            NifError::AllocFail =>
                unsafe { ruster_export::enif_make_badarg(env.as_c_arg()) },
            NifError::Atom(name) => 
                unsafe { ruster_export::enif_make_atom_len(env.as_c_arg(), 
                                                           name.as_ptr() as *const u8, 
                                                           name.len() as size_t) },
        })
    }
}

#[derive(Clone, Copy)]
pub struct NifTerm<'a> {
    pub term: ERL_NIF_TERM,
    env_life: PhantomData<&'a NifEnv>,
}
impl<'a> NifTerm<'a> {
    pub fn new(_env: &'a NifEnv, inner: ERL_NIF_TERM) -> Self {
        NifTerm {
            term: inner,
            env_life: PhantomData
        }
    }
    pub fn as_c_arg(&self) -> ERL_NIF_TERM {
        self.term
    }
}

pub struct NifResourceType {
    pub res: *mut ErlNifResourceType
}
pub struct NifStructResourceType<T> {
    pub res: NifResourceType,
    pub struct_type: PhantomData<T>,
}



pub fn decode_type<T: NifDecoder>(term: NifTerm, env: &NifEnv) -> Result<T, NifError> {
    NifDecoder::decode(term, env)
}

#[macro_export]
macro_rules! nif_atom {
    ($env:expr, $name:ident) => ({
        const atom_name: &'static str = stringify!($name);
        $crate::ruster_export::enif_make_atom_len(
            $env.env,
            atom_name.as_ptr() as *const u8,
            atom_name.len() as $crate::size_t)
    });
}

pub fn nif_atom<'a>(env: &'a NifEnv, name: &str) -> NifTerm<'a> {
    unsafe { 
        NifTerm::new(env, ruster_unsafe::enif_make_atom_len(
            env.env,
            name.as_ptr() as *const u8,
            name.len() as size_t))
    }
}

#[macro_export]
macro_rules! nif_func {
    ($name:ident, $fun:expr) => (
        extern "C" fn $name(r_env: *mut $crate::ruster_export::ErlNifEnv,
                            argc: $crate::c_int,
                            argv: *const $crate::ERL_NIF_TERM) -> $crate::ERL_NIF_TERM {
            use $crate::{ NifEnv, NifTerm, NifError, size_t };
            let env = NifEnv { env: r_env };

            let terms = unsafe { ::std::slice::from_raw_parts(argv, argc as usize) }.iter()
                .map(|x| { NifTerm::new(&env, *x) }).collect::<Vec<NifTerm>>();

            let result: std::thread::Result<Result<NifTerm, NifError>> = std::panic::recover(|| {
                let inner_fun: &for<'a> Fn(&'a NifEnv, &Vec<NifTerm>) -> Result<NifTerm<'a>, NifError> = &$fun;
                //let res: Result<NifTerm, NifError> = inner_fun(&env, &terms);
                inner_fun(&env, &terms)
            });

            match result {
                Ok(res) => match res {
                    Ok(ret) => ret.as_c_arg(),
                    Err(err) => err.to_term(&env).as_c_arg(),
                },
                Err(err) => NifError::Atom("nif_panic").to_term(&env).as_c_arg(),
            }
            //match res {
            //    Ok(ret) => ret.as_c_arg(),
            //    Err(err) => err.to_term(&env).as_c_arg(),
            //}
        });
}

// This is the last level of rust safe rust code before the BEAM.
// No panics should go above this point, as they will unwrap into the C code and ruin the day.
pub fn handle_nif_call(function: for<'a> fn(&'a NifEnv, &Vec<NifTerm>) -> Result<NifTerm<'a>, NifError>, 
                       arity: usize, 
                       r_env: *mut ruster_export::ErlNifEnv, 
                       argc: c_int, 
                       argv: *const ruster_export::ERL_NIF_TERM) -> ruster_export::ERL_NIF_TERM {
    let env = NifEnv { env: r_env };
    
    let terms = unsafe { ::std::slice::from_raw_parts(argv, argc as usize) }.iter()
        .map(|x| NifTerm::new(&env, *x)).collect::<Vec<NifTerm>>();

    let result: std::thread::Result<Result<NifTerm, NifError>> = std::panic::recover(|| function(&env, &terms));

    match result {
        Ok(res) => match res {
            Ok(ret) => ret.as_c_arg(),
            Err(err) => err.to_term(&env).as_c_arg(),
        },
        Err(err) => NifError::Atom("nif_panic").to_term(&env).as_c_arg(),
    }
}

#[macro_export]
macro_rules! nif_func_args {
    ($name:ident, ($($args:ty),*), $fun:expr) => (
        extern "C" fn $name(r_env: *mut $crate::ruster_export::ErlNifEnv,
                            argc: $crate::c_int,
                            argv: *const $crate::ERL_NIF_TERM) -> $crate::ERL_NIF_TERM {
            use $crate::{ NifEnv, NifTerm, NifError, size_t };
            let env = NifEnv { env: r_env };

            let terms = unsafe { ::std::slice::from_raw_parts(argv, argc as usize) }.iter()
                .map(|x| { NifTerm::new(&env, *x) }).collect::<Vec<NifTerm>>();
            match decode_term_array_to_tuple!(&env, &terms, ($($args),*)) {
                Ok(args_tuple) => {
                    let inner_fun: &for<'a> Fn(&'a NifEnv, ($($args,)*)) -> 
                        Result<NifTerm<'a>, NifError> = &$fun;
                    let res: Result<NifTerm, NifError> = inner_fun(&env, args_tuple);

                    match res {
                        Ok(ret) => ret.term,
                        Err(err) => err.to_term(&env).as_c_arg(),
                    }
                },
                Err(err) => err.to_term(&env).as_c_arg(),
            }


        });
}

#[macro_export]
macro_rules! nif_init_func {
    ($name:expr, $arity:expr, $function:ident, $flags:expr) => {
        $crate::ruster_export::ErlNifFunc {
            name: $name as *const u8,
            arity: $arity,
            function: $function,
            flags: $flags,
        }
    };
    ($name:expr, $arity:expr, $function:ident) => {
        nif_init_func!($name, $arity, $function, 0)
    }
}

#[macro_export]
macro_rules! nif_init {
    (@count ()) => { 0 };
    (@count ($_e:expr)) => { 1 };
    (@count ($_e:expr, $($tail:tt)*)) => { 1 + nif_init!(@count ($($tail)*)) };

    ($module:expr, $load:expr, $reload:expr, $upgrade:expr, $unload:expr, [$($func:expr),*]) => {
        const NUM_FUNCS: usize = nif_init!(@count ($($func),*));
        const FUNCS: [$crate::ruster_export::ErlNifFunc; NUM_FUNCS] = [$($func),*];
        static mut NIF_ENTRY: $crate::ruster_export::ErlNifEntry = $crate::ruster_export::ErlNifEntry {
            major: $crate::ruster_export::NIF_MAJOR_VERSION,
            minor: $crate::ruster_export::NIF_MINOR_VERSION,
            name: $module as *const u8,
            num_of_funcs: NUM_FUNCS as $crate::c_int,
            funcs: &FUNCS as *const $crate::ruster_export::ErlNifFunc,
            load: $load,
            reload: $reload,
            upgrade: $upgrade,
            unload: $unload,
            vm_variant: b"beam.vanilla\0" as *const u8,
            options: 0,
        };

        #[no_mangle]
        pub extern "C" fn nif_init() -> *const $crate::ruster_export::ErlNifEntry {
            unsafe { &NIF_ENTRY }
        }
    }
}
