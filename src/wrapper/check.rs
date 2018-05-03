use ::wrapper::nif_interface::{ NIF_ENV, NIF_TERM };
use ::wrapper::nif_interface;

macro_rules! impl_check_fun {
    ($name:ident, $inner:path) => {
        pub unsafe fn $name(env: NIF_ENV, term: NIF_TERM) -> bool {
            $inner(env, term) == 1
        }
    }
}

impl_check_fun!(is_atom, nif_interface::enif_is_atom);
impl_check_fun!(is_binary, nif_interface::enif_is_binary);
impl_check_fun!(is_empty_list, nif_interface::enif_is_empty_list);
impl_check_fun!(is_exception, nif_interface::enif_is_exception);
impl_check_fun!(is_fun, nif_interface::enif_is_fun);
impl_check_fun!(is_list, nif_interface::enif_is_list);
impl_check_fun!(is_map, nif_interface::enif_is_map);
impl_check_fun!(is_number, nif_interface::enif_is_number);
impl_check_fun!(is_pid, nif_interface::enif_is_pid);
impl_check_fun!(is_port, nif_interface::enif_is_port);
impl_check_fun!(is_ref, nif_interface::enif_is_ref);
impl_check_fun!(is_tuple, nif_interface::enif_is_tuple);
