//! Functions used by runtime generated code. Should not be used.

use std::ffi::CString;
use std::fmt;

use crate::{Encoder, Env, OwnedBinary, Term};

// Names used by the `rustler::init!` macro or other generated code.
pub use crate::wrapper::exception::raise_exception;
pub use crate::wrapper::{
    c_int, c_void, get_nif_resource_type_init_size, DEF_NIF_ENTRY, DEF_NIF_FUNC,
    MUTABLE_NIF_RESOURCE_HANDLE, NIF_ENV, NIF_MAJOR_VERSION, NIF_MINOR_VERSION, NIF_TERM,
};

#[cfg(windows)]
pub use rustler_sys::{TWinDynNifCallbacks, WIN_DYN_NIF_CALLBACKS};

pub unsafe trait NifReturnable {
    unsafe fn as_returned(self, env: Env) -> NifReturned;
}

unsafe impl<T> NifReturnable for T
where
    T: crate::Encoder,
{
    unsafe fn as_returned(self, env: Env) -> NifReturned {
        NifReturned::Term(self.encode(env).as_c_arg())
    }
}

unsafe impl<T> NifReturnable for Result<T, crate::error::Error>
where
    T: NifReturnable,
{
    unsafe fn as_returned(self, env: Env) -> NifReturned {
        match self {
            Ok(inner) => inner.as_returned(env),
            Err(inner) => inner.as_returned(env),
        }
    }
}

unsafe impl NifReturnable for OwnedBinary {
    unsafe fn as_returned(self, env: Env) -> NifReturned {
        NifReturned::Term(self.release(env).encode(env).as_c_arg())
    }
}

pub enum NifReturned {
    Term(NIF_TERM),
    Raise(NIF_TERM),
    BadArg,
    Reschedule {
        fun_name: CString,
        flags: crate::schedule::SchedulerFlags,
        fun: unsafe extern "C" fn(NIF_ENV, i32, *const NIF_TERM) -> NIF_TERM,
        args: Vec<NIF_TERM>,
    },
}

impl NifReturned {
    pub unsafe fn apply(self, env: Env) -> NIF_TERM {
        match self {
            NifReturned::Term(inner) => inner,
            NifReturned::BadArg => crate::wrapper::exception::raise_badarg(env.as_c_arg()),
            NifReturned::Raise(inner) => {
                crate::wrapper::exception::raise_exception(env.as_c_arg(), inner)
            }
            NifReturned::Reschedule {
                fun_name,
                flags,
                fun,
                args,
            } => rustler_sys::enif_schedule_nif(
                env.as_c_arg(),
                fun_name.as_ptr() as *const u8,
                flags as i32,
                fun,
                args.len() as i32,
                args.as_ptr() as *const usize,
            ),
        }
    }
}

impl fmt::Debug for NifReturned {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            NifReturned::BadArg => write!(fmt, "{{error, badarg}}"),
            NifReturned::Term(ref s) => write!(fmt, "{{ok, {}}}", s),
            NifReturned::Raise(ref s) => write!(fmt, "throw({})", s),
            NifReturned::Reschedule { .. } => write!(fmt, "reschedule()"),
        }
    }
}

/// # Unsafe
///
/// This takes arguments, including raw pointers, that must be correct.
pub unsafe fn handle_nif_init_call(
    function: Option<for<'a> fn(Env<'a>, Term<'a>) -> bool>,
    r_env: NIF_ENV,
    load_info: NIF_TERM,
) -> c_int {
    let env = Env::new(&(), r_env);
    let term = Term::new(env, load_info);

    if let Some(inner) = function {
        if inner(env, term) {
            0
        } else {
            1
        }
    } else {
        0
    }
}

pub fn handle_nif_result<T>(
    result: std::thread::Result<Result<T, crate::error::Error>>,
    env: Env,
) -> NifReturned
where
    T: NifReturnable,
{
    unsafe {
        match result {
            Ok(res) => match res {
                Ok(res) => NifReturnable::as_returned(res, env),
                Err(err) => NifReturnable::as_returned(err, env),
            },
            Err(err) => match err.downcast::<NifReturned>() {
                Ok(ty) => NifReturned::Term(ty.apply(env)),
                Err(_) => {
                    let term = crate::types::atom::nif_panicked().as_c_arg();
                    NifReturned::Raise(term)
                }
            },
        }
    }
}
