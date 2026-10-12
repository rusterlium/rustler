use std::rc::Rc;

#[rustler::nif]
async fn non_send() -> usize {
    let value = Rc::new(42);
    rustler::yield_now().await;
    *value
}

fn main() {}
