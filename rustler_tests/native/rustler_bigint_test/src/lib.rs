use rustler::{BigInt, NifResult};

#[rustler::nif]
pub fn echo(input: BigInt) -> NifResult<BigInt> {
    Ok(input)
}

#[rustler::nif]
pub fn add_one(input: BigInt) -> NifResult<BigInt> {
    Ok(input.checked_add(&BigInt::from(1)).unwrap())
}

#[rustler::nif]
pub fn add(a: BigInt, b: BigInt) -> NifResult<BigInt> {
    Ok(a.checked_add(&b).unwrap())
}

rustler::init!("Elixir.RustlerBigintTest");
