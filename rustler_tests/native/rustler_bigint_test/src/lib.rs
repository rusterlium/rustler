use rustler::NifResult;
use rustler_bigint::num_bigint;
use rustler_bigint::BigInt;

#[rustler::nif]
pub fn echo(input: BigInt) -> NifResult<BigInt> {
    Ok(input)
}

#[rustler::nif]
pub fn add_one(input: BigInt) -> NifResult<BigInt> {
    Ok(input
        .checked_add(&num_bigint::BigInt::from_signed_bytes_be(&[1]))
        .unwrap()
        .into())
}

#[rustler::nif]
pub fn add(a: BigInt, b: BigInt) -> NifResult<BigInt> {
    Ok(a.checked_add(&b).unwrap().into())
}

rustler::init!("Elixir.RustlerBigintTest", [echo, add_one, add]);
