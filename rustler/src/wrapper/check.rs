use crate::wrapper::{NIF_ENV, NIF_TERM};

macro_rules! impl_check_fun {
    ($name:ident, $inner:path) => {
        pub unsafe fn $name(env: NIF_ENV, term: NIF_TERM) -> bool {
            $inner(env, term) == 1
        }
    };
}

impl_check_fun!(is_atom, erl_nif_sys::enif_is_atom);
impl_check_fun!(is_binary, erl_nif_sys::enif_is_binary);
impl_check_fun!(is_empty_list, erl_nif_sys::enif_is_empty_list);
impl_check_fun!(is_exception, erl_nif_sys::enif_is_exception);
impl_check_fun!(is_fun, erl_nif_sys::enif_is_fun);
impl_check_fun!(is_list, erl_nif_sys::enif_is_list);
impl_check_fun!(is_map, erl_nif_sys::enif_is_map);
impl_check_fun!(is_number, erl_nif_sys::enif_is_number);
impl_check_fun!(is_pid, erl_nif_sys::enif_is_pid);
impl_check_fun!(is_port, erl_nif_sys::enif_is_port);
impl_check_fun!(is_ref, erl_nif_sys::enif_is_ref);
impl_check_fun!(is_tuple, erl_nif_sys::enif_is_tuple);
