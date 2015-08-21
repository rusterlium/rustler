extern crate ruster_unsafe;
pub use self::ruster_unsafe::{ ERL_NIF_TERM, ErlNifResourceFlags, ErlNifResourceType };

extern crate libc;
pub use libc::{ c_char, size_t, c_int, c_uint, c_void };

use std::mem;
use std::marker::PhantomData;

mod types;
pub use self::types::{ NifEncoder, NifDecoder };

mod resource;
pub use self::resource::{ open_resource_type_raw, alloc_resource_raw };
pub use self::resource::{ open_struct_resource_type, alloc_struct_resource, get_struct_resource };

mod binary;
pub use self::binary::{ NifBinary, alloc_binary, make_binary, get_binary };

pub struct NifEnv {
    pub env: *mut ruster_unsafe::ErlNifEnv,
}
impl NifEnv {
    unsafe fn as_c_arg(&self) -> *mut ruster_unsafe::ErlNifEnv {
        self.env
    }
}

#[derive(Clone, Copy)]
pub enum NifError {
    BadArg,
    AllocFail,
    Atom(&'static str),
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
    unsafe fn as_c_arg(&self) -> ERL_NIF_TERM {
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

// Atoms are a special case of a term. They can be stored and used on all envs regardless of where
// it lives and when it is created.
#[derive(PartialEq, Eq)]
pub struct NifAtom {
    term: ERL_NIF_TERM,
}

pub fn get_tuple<'a>(env: &'a NifEnv, term: NifTerm) -> Result<Vec<NifTerm<'a>>, NifError> {
    let mut arity: c_int = 0;
    let mut array_ptr: *const ERL_NIF_TERM = unsafe { mem::uninitialized() };
    let success = unsafe { ruster_unsafe::enif_get_tuple(env.env, term.term, 
                                                         &mut arity as *mut c_int, 
                                                         &mut array_ptr as *mut *const ERL_NIF_TERM) };
    if success == 0 {
        return Err(NifError::BadArg);
    }
    let term_array = unsafe { std::slice::from_raw_parts(array_ptr, arity as usize) };
    Ok(term_array.iter().map(|x| { NifTerm::new(env, *x) }).collect::<Vec<NifTerm>>())
}

pub fn decode_type<T: NifDecoder>(term: NifTerm, env: &NifEnv) -> Result<T, NifError> {
    NifDecoder::decode(term, env)
}

#[macro_export]
macro_rules! decode_tuple {
    (@count ()) => { 0 };
    (@count ($_i:ty, $($rest:tt)*)) => { 1 + decode_tuple!(@count ($($rest)*)) };
    (@accum $_env:expr, $_list:expr, $_num:expr, ($(,)*) -> ($($body:tt)*)) => {
        decode_tuple!(@as_expr ($($body)*))
    };
    (@accum $env:expr, $list:expr, $num:expr, ($head:ty, $($tail:tt)*) -> ($($body:tt)*)) => {
        decode_tuple!(@accum $env, $list, ($num+1), ($($tail)*) -> ($($body)* decode_tuple!(@decode_arg $env, $head, $list[$num]),))
    };
    (@as_expr $e:expr) => {$e};
    (@decode_arg $env:expr, $typ:ty, $val:expr) => {
        match $crate::nif::decode_type::<$typ>($val, $env) {
            Ok(val) => val,
            Err(val) => return Err(val),
        }
    };
    ($env:expr, $term:expr, ($($typs:ty),*)) => {
        {
            let num_expr: usize = decode_tuple!(@count ($($typs,)*));
            let terms = try!($crate::nif::get_tuple($env, $term));
            if terms.len() != num_expr {
                Err($crate::nif::NifError::BadArg)
            } else {
                Ok(decode_tuple!(@accum $env, terms, 0, ($($typs),*,) -> ()))
            }
        }
    }
}

//macro_rules! decode_tuple {
//    ($env:expr, $term:expr, ($($typ:ty),+)) => {
//        let num_expr: usize = count_typ!($($typ,)*);
//        let terms = try!($crate::nif::get_tuple($env, $term));
//        if terms.len() != num_expr {
//            Err($crate::nif::NifError::BadArg)
//        } else {
//            Ok((decode_tuple_arg!($env, terms, 0, ($($typ,)*))))
//        }
//    }
//}

#[macro_export]
macro_rules! nif_func {
    ($name:ident, $fun:expr) => (
        extern "C" fn $name(r_env: *mut ErlNifEnv,
                            argc: c_int,
                            argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM {
            use $crate::nif::{ NifEnv, NifTerm, NifError, size_t };
            let env = NifEnv { env: r_env };
            let terms = unsafe { std::slice::from_raw_parts(argv, argc as usize) }.iter()
                .map(|x| { NifTerm::new(&env, *x) }).collect::<Vec<NifTerm>>();
            let inner_fun: &for<'a> Fn(&'a NifEnv, &Vec<NifTerm>) -> Result<NifTerm<'a>, NifError> = &$fun;
            let res: Result<NifTerm, NifError> = inner_fun(&env, &terms);
            match res {
                Ok(ret) => ret.term,
                Err(NifError::BadArg) => 
                    unsafe { ruster_unsafe::enif_make_badarg(r_env) },
                Err(NifError::Atom(name)) => 
                    unsafe { ruster_unsafe::enif_make_atom_len(r_env, 
                                                               name.as_ptr() as *const u8, 
                                                               name.len() as size_t) },
            }
        });
}

#[macro_export]
macro_rules! nif_atom {
    ($env:expr, $name:ident) => ({
        const atom_name: &'static str = stringify!($name);
        ruster_unsafe::enif_make_atom_len(
            $env.env,
            atom_name.as_ptr() as *const u8,
            atom_name.len() as $crate::nif::size_t)
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
macro_rules! nif_func_entry {
    ($name:expr, $arity:expr, $function:ident, $flags:expr) => {
        {
            let c_name = $crate::std::ffi::CString::new($name).unwrap();
            $crate::ruster_unsafe::ErlNifFunc {
                name: c_name.as_ptr(),
                arity: $arity,
                function: $function,
                flags: $flags,
            };
        }
    };
    ($name:expr, $arity:expr, $function:expr, $flags:expr) => {
        nif_func_entry
    }
}

#[macro_export]
macro_rules! nif_init {
    ($module:expr, $load:expr, $reload:expr, $upgrade:expr, $unload:expr, [$($func:expr),*]) => {
        const NUM_FUNCS: usize = nif_init!(@count_expr $($func),*);
        const FUNCS: [$crate::ruster_unsafe::ErlNifFunc; NUM_FUNCS] = [$($func),*];
        
        let c_module = $crate::std::ffi::CString::new($module).unwrap();

        static mut ENTRY: $crate::ruster_unsafe::ErlNifEntry = $crate::ErlNifEntry {
            major: $crate::ruster_unsafe::NIF_MAJOR_VERSION,
            minor: $crate::ruster_unsafe::NIF_MINOR_VERSION,
            name: c_module,
            num_of_funcs: NUM_FUNCS as $crate::c_int,
            funcs: &FUNCS as *const $crate::ruster_unsafe::ErlNifFunc,
            load: $load,
            reload: $reload,
            upgrade: $upgrade,
            unload: $unload,
            vm_variant: b"beam.vanilla\0" as *const u8,
            options: 0,
        };

        #[no_mangle]
        pub extern "C" fn nif_init() -> *const $crate::ruster_unsafe::ErlNifEntry {
            unsafe { &ENTRY }
        }
    };

    (@count_expr ()) => { 0 };
    (@count_expr ($_e:expr)) => { 1 };
    (@count_expr ($_e:expr, $($tail:tt)*)) => { 1 + nif_init!(@count_expr $($rest)*) }
}
