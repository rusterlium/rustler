#![allow(deprecated)]
use rustler::{Encoder, Env, Error, Term};

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
    }
}

#[allow(dead_code)]
struct TestResource {
    a: u32,
}

rustler::rustler_export_nifs! {
    "Elixir.DeprecatedMacros",
    [
        ("add", 2, add),
    ],
    Some(load)
}

fn add<'a>(env: Env<'a>, terms: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let a: u32 = terms[0].decode()?;
    let b: u32 = terms[1].decode()?;

    Ok((atoms::ok(), a + b).encode(env))
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource_struct_init!(TestResource, env);
    true
}
