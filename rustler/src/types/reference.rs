use crate::{Env, Term, TermType};

use crate::sys::enif_make_ref;

wrapper!{
    struct Reference(TermType::Ref)
}

impl<'a> Env<'a> {
    /// Create a new reference in this environment
    pub fn make_ref(self) -> Reference<'a> {
        let term = unsafe { Term::new(self, enif_make_ref(self.as_c_arg())) };
        unsafe { Reference::wrap_unchecked(term) }
    }
}
