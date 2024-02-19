use crate::codegen_runtime::{c_char, c_int, c_uint, DEF_NIF_FUNC, NIF_ENV, NIF_TERM};

pub trait Nif {
    const NAME: *const c_char;
    const ARITY: c_uint;
    const FLAGS: c_uint;
    const FUNC: DEF_NIF_FUNC;
    const RAW_FUNC: unsafe extern "C" fn(
        nif_env: NIF_ENV,
        argc: c_int,
        argv: *const NIF_TERM,
    ) -> NIF_TERM;
}
