use rustler::{NifEnv, NifTerm, NifResult, NifEncoder};
use std::io::Write;

pub fn term_debug<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let mut bytes: Vec<u8> = Vec::new();
    write!(&mut bytes, "{:?}", args[0]).expect("debug formatting should succeed");
    let s = String::from_utf8_lossy(&bytes).to_string();
    Ok(s.encode(env))
}
