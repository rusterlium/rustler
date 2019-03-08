use rustler::{Env, Term, NifResult};
use rustler::Atom;
use std::io::Write;
use std::cmp::Ordering;

pub fn term_debug<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<String> {
    let mut bytes: Vec<u8> = Vec::new();
    write!(&mut bytes, "{:?}", args[0]).expect("debug formatting should succeed");
    Ok(String::from_utf8_lossy(&bytes).to_string())
}

mod atoms {
    rustler::rustler_atoms! {
        atom equal;
        atom less;
        atom greater;
    }
}

pub fn term_eq<'a>(_env: Env<'a>, args: &[Term<'a>]) -> bool {
    args[0] == args[1]
}
pub fn term_cmp<'a>(_env: Env<'a>, args: &[Term<'a>]) -> Atom {
    match Ord::cmp(&args[0], &args[1]) {
        Ordering::Equal => atoms::equal(),
        Ordering::Less => atoms::less(),
        Ordering::Greater => atoms::greater(),
    }
}
