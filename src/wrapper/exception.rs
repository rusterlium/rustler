use super::nif_interface::{ NIF_ENV, NIF_TERM };
use super::nif_interface;

pub fn throw(env: NIF_ENV, term: NIF_TERM) -> NIF_TERM {
    unsafe { nif_interface::enif_raise_exception(env, term) }
}
