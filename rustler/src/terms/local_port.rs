//! Environment-independent local port identifiers.
//!
//! External ports are represented by [`super::Port`] instead.

free_term!(LocalPort, |term: crate::Term<'_>| {
    let mut port = std::mem::MaybeUninit::<crate::sys::ErlNifPort>::uninit();
    unsafe {
        crate::sys::enif_get_local_port(
            term.get_env().as_c_arg(),
            term.as_c_arg(),
            port.as_mut_ptr(),
        ) != 0
    }
});
