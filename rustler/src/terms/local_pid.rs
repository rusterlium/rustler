//! Environment-independent local process identifiers.
//!
//! Remote pids are represented by [`super::Pid`] instead.

free_term!(LocalPid, |term: crate::Term<'_>| {
    term.decode::<crate::LocalPid>().is_ok()
});
