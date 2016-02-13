use super::{ NifEnv, NifTerm, NifError };

pub fn get_tuple<'a>(term: NifTerm<'a>) -> Result<Vec<NifTerm<'a>>, NifError> {
    match ::wrapper::get_tuple(term.env.as_c_arg(), term.as_c_arg()) {
        Ok(terms) => Ok(terms.iter().map(|x| { unsafe { NifTerm::new(term.env, *x) } }).collect::<Vec<NifTerm>>()),
        Err(_error) => Err(NifError::BadArg)
    }
}

pub fn make_tuple<'a>(env: &'a NifEnv, terms: &[NifTerm]) -> NifTerm<'a> {
    let c_terms: Vec<::wrapper::nif_interface::NIF_TERM> = terms.iter().map(|term| term.as_c_arg()).collect();
    NifTerm::new(env, ::wrapper::tuple::make_tuple(env.as_c_arg(), &c_terms))
}

