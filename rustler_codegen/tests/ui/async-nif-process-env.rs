use rustler::{Env, Term};

#[rustler::nif]
async fn process_env<'a>(env: Env<'a>) -> Term<'a> {
    rustler::yield_now().await;
    env.error_tuple("invalid")
}

fn main() {}
