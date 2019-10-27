use rustler::{Encoder, Env, Error, Term};

mod atoms {
    rustler::atoms! {
        ok,
        // error,
        // __true__ = "true",
        // __false__ = "false"
    }
}

rustler::init! {
    "<%= native_module %>",
    [
        ("add", 2, add)
    ],
    None
}

fn add<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let num1: i64 = args[0].decode()?;
    let num2: i64 = args[1].decode()?;

    Ok((atoms::ok(), num1 + num2).encode(env))
}
