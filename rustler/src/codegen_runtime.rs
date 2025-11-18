//! Functions used by runtime generated code. Should not be used.

use std::ffi::CString;
use std::fmt;

use crate::types::atom;
use crate::{Encoder, Env, OwnedBinary, Term};

// Re-export of inventory
pub use inventory;

// Re-export of resource registration
pub use crate::resource::Registration as ResourceRegistration;

// Names used by the `rustler::init!` macro or other generated code.
pub use crate::wrapper::exception::raise_exception;
pub use crate::wrapper::{
    c_char, c_int, c_uint, c_void, get_nif_resource_type_init_size, DEF_NIF_ENTRY, DEF_NIF_FUNC,
    NIF_ENV, NIF_MAJOR_VERSION, NIF_MINOR_VERSION, NIF_TERM,
};

pub use crate::sys::{internal_set_symbols, internal_write_symbols, DynNifCallbacks};

pub unsafe trait NifReturnable {
    unsafe fn into_returned(self, env: Env) -> NifReturned;
}

unsafe impl<T> NifReturnable for T
where
    T: crate::Encoder + std::panic::RefUnwindSafe,
{
    unsafe fn into_returned(self, env: Env) -> NifReturned {
        if let Ok(res) = std::panic::catch_unwind(|| NifReturned::Term(self.encode(env).as_c_arg()))
        {
            res
        } else {
            let term = atom::nif_panicked().as_c_arg();
            NifReturned::Raise(term)
        }
    }
}

unsafe impl<T> NifReturnable for Result<T, crate::error::Error>
where
    T: NifReturnable,
{
    unsafe fn into_returned(self, env: Env) -> NifReturned {
        match self {
            Ok(inner) => inner.into_returned(env),
            Err(inner) => inner.into_returned(env),
        }
    }
}

unsafe impl NifReturnable for OwnedBinary {
    unsafe fn into_returned(self, env: Env) -> NifReturned {
        NifReturned::Term(self.release(env).encode(env).as_c_arg())
    }
}

// Allow returning NifReturned directly from NIFs
// This is useful for advanced use cases like yielding NIFs
unsafe impl NifReturnable for NifReturned {
    unsafe fn into_returned(self, _env: Env) -> NifReturned {
        self
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
            } => crate::sys::enif_schedule_nif(
                env.as_c_arg(),
                fun_name.as_ptr() as *const c_char,
                flags as i32,
                fun,
                args.len() as i32,
                args.as_ptr(),
            ),
        }
    }
}

impl fmt::Debug for NifReturned {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            NifReturned::BadArg => write!(fmt, "{{error, badarg}}"),
            NifReturned::Term(ref s) => write!(fmt, "{{ok, {s}}}"),
            NifReturned::Raise(ref s) => write!(fmt, "throw({s})"),
            NifReturned::Reschedule { .. } => write!(fmt, "reschedule()"),
        }
    }
}

/// # Unsafe
///
/// This takes arguments, including raw pointers, that must be correct.
pub unsafe fn handle_nif_init_call<'a>(
    function: for<'b> fn(Env<'b>, Term<'b>) -> bool,
    env: Env<'a>,
    load_info: Term<'a>,
) -> c_int {
    std::panic::catch_unwind(|| function(env, load_info)).map_or(1, |x| i32::from(!x))
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
                Ok(res) => NifReturnable::into_returned(res, env),
                Err(err) => NifReturnable::into_returned(err, env),
            },
            Err(err) => match err.downcast::<NifReturned>() {
                Ok(ty) => NifReturned::Term(ty.apply(env)),
                Err(_) => {
                    let term = atom::nif_panicked().as_c_arg();
                    NifReturned::Raise(term)
                }
            },
        }
    }
}

pub const fn min_erts() -> &'static [u8] {
    if cfg!(feature = "nif_version_2_17") {
        b"OTP-26.0\0"
    } else if cfg!(feature = "nif_version_2_16") {
        b"OTP-24.0\0"
    } else if cfg!(feature = "nif_version_2_15") {
        b"OTP-22.0\0"
    } else {
        b"OTP-21.0\0"
    }
}
