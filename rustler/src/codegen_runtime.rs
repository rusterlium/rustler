//! Functions used by runtime generated code. Should not be used.

use std::ffi::CString;
use std::fmt;

use crate::sys::{enif_make_badarg, enif_raise_exception, enif_schedule_nif};
use crate::types::atom;
use crate::{Encoder, Env, OwnedBinary, Term};

// Re-export of inventory
pub use inventory;

// Re-export of resource registration
pub use crate::resource::Registration as ResourceRegistration;

// Names used by the `rustler::init!` macro or other generated code.
pub use crate::sys::{c_char, c_int, c_uint, c_void};

pub use crate::sys::{
    internal_set_symbols, internal_write_symbols, DynNifCallbacks, ErlNifEntry, ErlNifEnv,
    ErlNifFunc, ErlNifTerm,
};

/// Writes the module of a library and its NIFs to the file that the
/// environment variable `RUSTLER_NIF_MANIFEST` names: the module on the first
/// line, then one line `<arity> <name>` for each NIF.
///
/// `erlang:load_nif/2` accepts a library only from the module that the library
/// names, and only when that module defines each NIF. The BEAM asks the library
/// for both before it checks them, so a tool that knows neither can call
/// `erlang:load_nif/2` from any module, read this file, and then load the
/// library from a module that fits. The first call fails, as it must.
///
/// Without the `nif_manifest` feature this does nothing.
///
/// # Safety
///
/// The name of each NIF is a C string.
pub unsafe fn write_manifest(module: &str, nifs: &[ErlNifFunc]) {
    #[cfg(feature = "nif_manifest")]
    if let Some(path) = std::env::var_os("RUSTLER_NIF_MANIFEST") {
        let mut manifest = format!("{module}\n");
        for nif in nifs {
            let name = std::ffi::CStr::from_ptr(nif.name).to_string_lossy();
            manifest.push_str(&format!("{} {name}\n", nif.arity));
        }
        // A tool that finds no file reports that.
        let _ = std::fs::write(path, manifest);
    }
    #[cfg(not(feature = "nif_manifest"))]
    let _ = (module, nifs);
}

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

pub enum NifReturned {
    Term(ErlNifTerm),
    Raise(ErlNifTerm),
    BadArg,
    Reschedule {
        fun_name: CString,
        flags: crate::schedule::SchedulerFlags,
        fun: unsafe extern "C" fn(*mut ErlNifEnv, i32, *const ErlNifTerm) -> ErlNifTerm,
        args: Vec<ErlNifTerm>,
    },
}

impl NifReturned {
    pub unsafe fn apply(self, env: Env) -> ErlNifTerm {
        match self {
            NifReturned::Term(inner) => inner,
            NifReturned::BadArg => enif_make_badarg(env.as_c_arg()),
            NifReturned::Raise(inner) => enif_raise_exception(env.as_c_arg(), inner),
            NifReturned::Reschedule {
                fun_name,
                flags,
                fun,
                args,
            } => enif_schedule_nif(
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
