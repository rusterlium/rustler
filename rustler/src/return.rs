use crate::codegen_runtime::{NifReturnable, NifReturned};
use crate::error::Error;
use crate::{Env, Term};

pub enum Return<'a> {
    Term(Term<'a>),
    Error(Error),
}

unsafe impl<'b> NifReturnable for Return<'b> {
    unsafe fn as_returned(self, env: Env) -> NifReturned {
        match self {
            Return::Term(inner) => NifReturned::Term(inner.as_c_arg()),
            Return::Error(inner) => inner.as_returned(env),
        }
    }
}
