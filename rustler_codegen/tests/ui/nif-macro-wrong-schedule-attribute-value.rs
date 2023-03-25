use rustler_codegen::nif;

#[nif(schedule = "DirtyGPU")]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() {}
