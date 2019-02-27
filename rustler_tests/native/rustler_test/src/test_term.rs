use rustler::{Atom, Term};
use std::cmp::Ordering;
use std::io::Write;

mod atoms {
    rustler::atoms! {
        equal,
        less,
        greater,
    }
}

#[rustler::nif]
pub fn term_debug(term: Term) -> String {
    let mut bytes: Vec<u8> = Vec::new();
    write!(&mut bytes, "{:?}", term).expect("debug formatting should succeed");
    String::from_utf8_lossy(&bytes).to_string()
}

#[rustler::nif]
pub fn term_eq<'a>(a: Term<'a>, b: Term<'a>) -> bool {
    a == b
}

#[rustler::nif]
pub fn term_cmp<'a>(a: Term<'a>, b: Term<'a>) -> Atom {
    match Ord::cmp(&a, &b) {
        Ordering::Equal => atoms::equal(),
        Ordering::Less => atoms::less(),
        Ordering::Greater => atoms::greater(),
    }
}

#[rustler::nif]
pub fn term_internal_hash(term: Term, salt: u32) -> u32 {
    term.hash_internal(salt)
}

#[rustler::nif]
pub fn term_phash2_hash(term: Term) -> u32 {
    term.hash_phash2()
}
