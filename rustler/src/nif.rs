use crate::codegen_runtime::{c_char, c_int, c_uint, DEF_NIF_FUNC};
use crate::sys::{ErlNifEnv, ERL_NIF_TERM};

pub struct Nif {
    pub name: *const c_char,
    pub arity: c_uint,
    pub flags: c_uint,
    // pub func: DEF_NIF_FUNC,
    pub raw_func: unsafe extern "C" fn(
        nif_env: *mut ErlNifEnv,
        argc: c_int,
        argv: *const ERL_NIF_TERM,
    ) -> ERL_NIF_TERM,
}

impl Nif {
    pub fn get_def(&self) -> DEF_NIF_FUNC {
        DEF_NIF_FUNC {
            arity: self.arity,
            flags: self.flags,
            function: self.raw_func,
            name: self.name,
        }
    }
}

unsafe impl Sync for Nif {}

inventory::collect!(Nif);
