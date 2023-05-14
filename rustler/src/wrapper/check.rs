use crate::wrapper::{NIF_ENV, NIF_TERM};

macro_rules! impl_check_fun {
    ($name:ident, $inner:path) => {
        pub unsafe fn $name(env: NIF_ENV, term: NIF_TERM) -> bool {
            $inner(env, term) == 1
        }
    };
}

impl_check_fun!(is_atom, rustler_sys::enif_is_atom);
impl_check_fun!(is_binary, rustler_sys::enif_is_binary);
impl_check_fun!(is_empty_list, rustler_sys::enif_is_empty_list);
impl_check_fun!(is_fun, rustler_sys::enif_is_fun);
impl_check_fun!(is_list, rustler_sys::enif_is_list);
impl_check_fun!(is_map, rustler_sys::enif_is_map);
impl_check_fun!(is_number, rustler_sys::enif_is_number);
impl_check_fun!(is_pid, rustler_sys::enif_is_pid);
impl_check_fun!(is_port, rustler_sys::enif_is_port);
impl_check_fun!(is_ref, rustler_sys::enif_is_ref);
impl_check_fun!(is_tuple, rustler_sys::enif_is_tuple);
