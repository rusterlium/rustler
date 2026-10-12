use rustler::{Env, ProcessIndependent};

#[rustler::nif]
async fn copy_owned_env<'a>(env: Env<'a, ProcessIndependent>) -> () {
    let other = env;
    rustler::yield_now().await;
    let _ = env.error_tuple("moved");
    let _ = other.error_tuple("owned");
}

fn main() {}
