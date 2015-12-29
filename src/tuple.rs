use super::{ NifEnv, NifTerm, NifError };
use super::{ c_int };

use std::mem;

extern crate ruster_unsafe;
use ruster_unsafe::{ ERL_NIF_TERM };

/*pub fn get_tuple<'a>(env: &'a NifEnv, term: NifTerm) -> Result<Vec<NifTerm<'a>>, NifError> {
    let mut arity: c_int = 0;
    let mut array_ptr: *const ERL_NIF_TERM = unsafe { mem::uninitialized() };
    let success = unsafe { ruster_unsafe::enif_get_tuple(env.env, term.term, 
                                                         &mut arity as *mut c_int, 
                                                         &mut array_ptr as *mut *const ERL_NIF_TERM) };
    if success != 1 {
        return Err(NifError::BadArg);
    }
    let term_array = unsafe { ::std::slice::from_raw_parts(array_ptr, arity as usize) };
    Ok(term_array.iter().map(|x| { NifTerm::new(env, *x) }).collect::<Vec<NifTerm>>())
}*/

pub fn get_tuple<'a>(env: &'a NifEnv, term: NifTerm) -> Result<Vec<NifTerm<'a>>, NifError> {
    match ::wrapper::get_tuple(env.as_c_arg(), term.as_c_arg()) {
        Ok(terms) => Ok(terms.iter().map(|x| { NifTerm::new(env, *x) }).collect::<Vec<NifTerm>>()),
        Err(error) => Err(NifError::BadArg)
    }
}

#[macro_export]
macro_rules! decode_term_array_to_tuple {
    (@count ()) => { 0 };
    (@count ($_i:ty, $($rest:tt)*)) => { 1 + decode_term_array_to_tuple!(@count ($($rest)*)) };
    (@accum $_env:expr, $_list:expr, $_num:expr, ($(,)*) -> ($($body:tt)*)) => {
        decode_term_array_to_tuple!(@as_expr ($($body)*))
    };
    (@accum $env:expr, $list:expr, $num:expr, ($head:ty, $($tail:tt)*) -> ($($body:tt)*)) => {
        decode_term_array_to_tuple!(@accum $env, $list, ($num+1), ($($tail)*) -> ($($body)* decode_term_array_to_tuple!(@decode_arg $env, $head, $list[$num]),))
    };
    (@as_expr $e:expr) => {$e};
    (@decode_arg $env:expr, $typ:ty, $val:expr) => {
        match $crate::decode_type::<$typ>($val, $env) {
            Ok(val) => val,
            Err(val) => return Err(val),
        }
    };
    ($env:expr, $terms:expr, ($($typs:ty),*)) => {
        {
            let decoder: &Fn(&NifEnv, &[NifTerm]) -> Result<($($typs),*), NifError> = &|env, terms| {
                let num_expr: usize = decode_term_array_to_tuple!(@count ($($typs,)*));
                if $terms.len() != num_expr {
                    Err($crate::NifError::BadArg)
                } else {
                    Ok(decode_term_array_to_tuple!(@accum $env, $terms, 0, ($($typs),*,) -> ()))
                }
            };
            decoder($env, $terms)
        }
    }
}

#[macro_export]
macro_rules! decode_tuple {
    ($env:expr, $term:expr, ($($typs:ty),*)) => {
        {
            let terms = try!($crate::tuple::get_tuple($env, $term));
            decode_term_array_to_tuple!($env, &terms[..], ($($typs),*))
        }
    }
}
