use crate::wrapper::{NIF_ENV, NIF_TERM};

macro_rules! impl_check_fun {
    ($name:ident, $inner:ident) => {
        pub unsafe fn $name(env: NIF_ENV, term: NIF_TERM) -> bool {
            crate::sys::$inner(env, term) == 1
        }
    };
}

impl_check_fun!(is_atom, enif_is_atom);
impl_check_fun!(is_binary, enif_is_binary);
impl_check_fun!(is_empty_list, enif_is_empty_list);
impl_check_fun!(is_fun, enif_is_fun);
impl_check_fun!(is_list, enif_is_list);
impl_check_fun!(is_map, enif_is_map);
impl_check_fun!(is_number, enif_is_number);
impl_check_fun!(is_pid, enif_is_pid);
impl_check_fun!(is_port, enif_is_port);
impl_check_fun!(is_ref, enif_is_ref);
impl_check_fun!(is_tuple, enif_is_tuple);
