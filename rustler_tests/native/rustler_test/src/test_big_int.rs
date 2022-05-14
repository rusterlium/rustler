
use rustler::types::big_int::BigInt;

#[rustler::nif]
pub fn big_int_add_one(big_int: BigInt) -> BigInt {
    big_int + BigInt::from(1)
}
