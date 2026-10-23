//! `NonZero*` integers, encoded as their integer; decoding rejects zero.

use crate::{Decoder, Encoder, Env, Error, NifResult, Term};
use std::num::{
    NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU128,
    NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize,
};

macro_rules! impl_nonzero {
    ($($nonzero:ty => $int:ty),* $(,)?) => {
        $(
            impl Encoder for $nonzero {
                fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
                    self.get().encode(env)
                }
            }

            impl<'a> Decoder<'a> for $nonzero {
                fn decode(term: Term<'a>) -> NifResult<$nonzero> {
                    <$nonzero>::new(term.decode::<$int>()?).ok_or(Error::BadArg)
                }
            }
        )*
    };
}

impl_nonzero!(
    NonZeroU8 => u8,
    NonZeroU16 => u16,
    NonZeroU32 => u32,
    NonZeroU64 => u64,
    NonZeroU128 => u128,
    NonZeroUsize => usize,
    NonZeroI8 => i8,
    NonZeroI16 => i16,
    NonZeroI32 => i32,
    NonZeroI64 => i64,
    NonZeroI128 => i128,
    NonZeroIsize => isize,
);
