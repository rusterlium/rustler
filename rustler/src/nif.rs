use crate::codegen_runtime::{c_char, c_int, c_uint, DEF_NIF_FUNC, NIF_ENV, NIF_TERM};

#[repr(C)]
pub struct Nif {
    pub name: *const c_char,
    pub arity: c_uint,
    pub flags: c_uint,
    // pub func: DEF_NIF_FUNC,
    pub raw_func:
        unsafe extern "C" fn(nif_env: NIF_ENV, argc: c_int, argv: *const NIF_TERM) -> NIF_TERM,
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

#[no_mangle]
#[linkme::distributed_slice]
pub static RUSTLER_NIFS: [Nif];
