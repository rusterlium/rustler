fn main() {
    let mut owned = rustler::OwnedEnv::new();
    let env = owned.borrow();
    let term = env.encode("borrowed");
    owned.clear();
    let _ = term.decode::<String>();
}
