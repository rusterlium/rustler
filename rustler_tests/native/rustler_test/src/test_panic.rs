use rustler::{Decoder, Encoder, Env, Term};

#[rustler::nif]
pub fn panic_in_nif() -> i32 {
    panic!("panic!")
}

struct Panicking;

impl Encoder for Panicking {
    fn encode<'a>(&self, _env: Env<'a>) -> Term<'a> {
        panic!("panic in encode!")
    }
}

impl<'a> Decoder<'a> for Panicking {
    fn decode(_term: Term<'a>) -> rustler::NifResult<Self> {
        panic!("panic in decode!")
    }
}

#[rustler::nif]
pub fn panic_in_encode() -> Panicking {
    Panicking
}

#[rustler::nif]
pub fn panic_in_decode(_p: Panicking) -> i32 {
    0
}
