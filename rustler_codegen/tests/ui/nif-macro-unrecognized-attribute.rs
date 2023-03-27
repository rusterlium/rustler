use rustler_codegen::nif;

#[nif(scheduler = "DirtyCpu")]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {}
