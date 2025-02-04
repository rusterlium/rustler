use crate::{
    sys::enif_make_list_from_array,
    wrapper::{NIF_ENV, NIF_TERM},
};
pub unsafe fn make_list(env: NIF_ENV, arr: &[NIF_TERM]) -> NIF_TERM {
    enif_make_list_from_array(env, arr.as_ptr(), arr.len() as u32)
}
