//! Functions used by runtime generated code. Should not be used.

use std::ffi::CString;

use crate::{Env, Term};

// Names used by the `rustler_export_nifs!` macro or other generated code.
pub use crate::wrapper::exception::raise_exception;
pub use crate::wrapper::nif_interface::{
    c_int, c_void, get_nif_resource_type_init_size, DEF_NIF_ENTRY, DEF_NIF_FUNC,
    MUTABLE_NIF_RESOURCE_HANDLE, NIF_ENV, NIF_MAJOR_VERSION, NIF_MINOR_VERSION, NIF_TERM,
};

#[cfg(windows)]
pub use erl_nif_sys::{TWinDynNifCallbacks, WIN_DYN_NIF_CALLBACKS};

pub unsafe trait NifReturnable {
    unsafe fn as_returned<'a>(self, env: Env<'a>) -> NifReturned;
}
unsafe impl<T> NifReturnable for T
where
    T: crate::Encoder,
{
    unsafe fn as_returned<'a>(self, env: Env<'a>) -> NifReturned {
        NifReturned::Term(self.encode(env).as_c_arg())
    }
}
unsafe impl<T> NifReturnable for Result<T, crate::error::Error>
where
    T: crate::Encoder,
{
    unsafe fn as_returned<'a>(self, env: Env<'a>) -> NifReturned {
        match self {
            Ok(inner) => NifReturned::Term(inner.encode(env).as_c_arg()),
            Err(inner) => inner.as_returned(env),
        }
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
    pub unsafe fn apply<'a>(self, env: Env<'a>) -> NIF_TERM {
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
            } => crate::wrapper::nif_interface::enif_schedule_nif(
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

/// # Unsafe
///
/// This takes arguments, including raw pointers, that must be correct.
pub unsafe fn handle_nif_init_call(
    function: Option<for<'a> fn(Env<'a>, Term<'a>) -> bool>,
    r_env: NIF_ENV,
    load_info: NIF_TERM,
) -> c_int {
    let env_lifetime = ();
    let env = Env::new(&env_lifetime, r_env);
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
