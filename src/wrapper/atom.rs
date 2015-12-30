use ::wrapper::nif_interface::{ NIF_ENV, NIF_TERM, enif_is_atom };

pub fn is_term_atom(env: NIF_ENV, term: NIF_TERM) -> bool {
    (unsafe { enif_is_atom(env, term) } == 1)
}
