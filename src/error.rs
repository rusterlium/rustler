use {types, wrapper, Encoder, Env, Term};

/// Represents usual errors that can happen in a nif. This enables you
/// to return an error from anywhere, even places where you don't have
/// an Env availible.
pub enum Error {
    /// Returned when the NIF has been called with the wrong number or type of
    /// arguments.
    BadArg,
    /// Encodes the string into an atom and returns it from the NIF.
    Atom(&'static str),
    RaiseAtom(&'static str),
    RaiseTerm(Box<Encoder>),
}

impl Error {
    /// # Unsafe
    ///
    /// If `self` is a `BadArg`, `RaiseAtom`, or `RaiseTerm` value, then the
    /// term returned from this method must not be used except as the return
    /// value from the calling NIF.
    pub unsafe fn encode<'a>(self, env: Env<'a>) -> Term<'a> {
        match self {
            Error::BadArg => {
                let exception = wrapper::exception::raise_badarg(env.as_c_arg());
                Term::new(env, exception)
            }
            Error::Atom(atom_str) => types::atom::Atom::from_str(env, atom_str)
                .ok()
                .expect("Error::Atom: bad atom")
                .to_term(env),
            Error::RaiseAtom(atom_str) => {
                let atom = types::atom::Atom::from_str(env, atom_str)
                    .ok()
                    .expect("Error::RaiseAtom: bad argument");
                let exception =
                    wrapper::exception::raise_exception(env.as_c_arg(), atom.as_c_arg());
                Term::new(env, exception)
            }
            Error::RaiseTerm(ref term_unencoded) => {
                let term = term_unencoded.encode(env);
                let exception =
                    wrapper::exception::raise_exception(env.as_c_arg(), term.as_c_arg());
                Term::new(env, exception)
            }
        }
    }
}
