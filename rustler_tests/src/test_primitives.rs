use rustler::{Env, Term, Encoder, NifResult};

pub fn add_u32<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let lhs: u32 = try!(args[0].decode());
    let rhs: u32 = try!(args[1].decode());

    Ok((lhs + rhs).encode(env))
}
pub fn add_i32<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let lhs: i32 = try!(args[0].decode());
    let rhs: i32 = try!(args[1].decode());

    Ok((lhs + rhs).encode(env))
}

pub fn echo_u8<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let num: u8 = try!(args[0].decode());
    Ok(num.encode(env))
}

pub fn option_inc<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let opt: Option<f64> = args[0].decode()?;
    let incremented = opt.and_then(|num| Some(num + 1.0));
    Ok(incremented.encode(env))
}

pub fn result_to_int<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let result: Result<bool, &'a str> = args[0].decode()?;
    let int_result = match result {
        Ok(true) => Ok(1),
        Ok(false) => Ok(0),
        Err(errstr) => Err(format!("{}{}", errstr, errstr)),
    };
    Ok(int_result.encode(env))
}
