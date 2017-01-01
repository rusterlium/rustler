use super::nif_interface::{ NIF_ENV, NIF_TERM };
use super::nif_interface;

pub fn raise_exception(env: NIF_ENV, term: NIF_TERM) -> NIF_TERM {
    unsafe { nif_interface::enif_raise_exception(env, term) }
}

pub fn raise_badarg(env: NIF_ENV) -> NIF_TERM {
    unsafe { nif_interface::enif_make_badarg(env) }
}
