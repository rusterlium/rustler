use rustler::{Error, NifResult};

mod atoms {
    rustler::atoms! {
        error,
        should_be_a_raised_term_as_atom,
        return_term_with_atom,
        should_be_an_atom_wrapped_in_an_error_tuple,
    }
}

#[rustler::nif]
pub fn bad_arg_error() -> NifResult<()> {
    Err(Error::BadArg)
}

#[rustler::nif]
pub fn atom_str_error() -> NifResult<()> {
    Err(Error::Atom("should_be_a_returned_atom"))
}

#[rustler::nif]
pub fn raise_atom_error() -> NifResult<()> {
    Err(Error::RaiseAtom("should_be_a_raised_atom"))
}

#[rustler::nif]
pub fn raise_term_with_string_error() -> NifResult<()> {
    Err(Error::RaiseTerm(Box::new(
        "should_be_a_raised_string".to_string(),
    )))
}

#[rustler::nif]
pub fn raise_term_with_atom_error() -> NifResult<()> {
    Err(Error::RaiseTerm(Box::new(
        atoms::should_be_a_raised_term_as_atom(),
    )))
}

#[rustler::nif]
pub fn term_with_tuple_error() -> NifResult<()> {
    let reason = atoms::should_be_an_atom_wrapped_in_an_error_tuple();
    Err(Error::Term(Box::new(reason)))
}
