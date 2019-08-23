use crate::wrapper::{NIF_ENV, NIF_TERM};

/// Raise an "error exception".
///
/// # Unsafe
///
/// The value returned by this function "can be used only as the return value
/// from the NIF that invoked it (directly or indirectly) or be passed to
/// `enif_is_exception`, but not to any other NIF API function."
///
/// And of course the usual rules about `env` and `term` still apply.
pub unsafe fn raise_exception(env: NIF_ENV, term: NIF_TERM) -> NIF_TERM {
    rustler_sys::enif_raise_exception(env, term)
}

/// Raise a `badarg` exception.
///
/// # Unsafe
///
/// The value returned by this function "can be used only as the return value
/// from the NIF that invoked it (directly or indirectly) or be passed to
/// `enif_is_exception`, but not to any other NIF API function."
///
/// And of course `env` must be a valid environment.
pub unsafe fn raise_badarg(env: NIF_ENV) -> NIF_TERM {
    rustler_sys::enif_make_badarg(env)
}
