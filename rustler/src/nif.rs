use crate::codegen_runtime::{c_int, DEF_NIF_FUNC, NIF_ENV, NIF_TERM};

pub trait Nif {
    const NAME: *const u8;
    const ARITY: u32;
    const FLAGS: u32;
    const FUNC: DEF_NIF_FUNC;
    const RAW_FUNC: unsafe extern "C" fn(
        nif_env: NIF_ENV,
        argc: c_int,
        argv: *const NIF_TERM,
    ) -> NIF_TERM;
}
