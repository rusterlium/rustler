//! Bitstring terms, including byte-aligned binaries.
//!
//! Requires NIF 2.15's term-type inspection API.

bound_term!(Bitstring, |term: crate::Term<'_>| {
    matches!(
        term.get_erl_type(),
        crate::sys::ErlNifTermType::ERL_NIF_TERM_TYPE_BITSTRING
    )
});
