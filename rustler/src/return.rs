use codegen_runtime::{NifReturnable, NifReturned};
use error::Error;
use {Env, Term};

pub enum Return<'a> {
    Term(Term<'a>),
    Error(Error),
}
unsafe impl<'b> NifReturnable for Return<'b> {
    unsafe fn as_returned<'a>(self, env: Env<'a>) -> NifReturned {
        match self {
            Return::Term(inner) => NifReturned::Term(inner.as_c_arg()),
            Return::Error(inner) => inner.as_returned(env),
        }
    }
}
