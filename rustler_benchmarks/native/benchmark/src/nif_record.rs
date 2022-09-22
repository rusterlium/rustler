#[derive(rustler_codegen::NifUnitEnum, Debug)]
pub enum Operation {
    DecodeAndEncode,
    Decode,
}

#[derive(rustler_codegen::NifRecord)]
#[tag = "my_record"]
pub struct MyRecord {
    a: i64,
    b: i64,
    c: i64,
    d: i64,
    e: i64,
    f: i64,
    g: i64,
    h: i64,
    i: i64,
    j: i64,
    k: i64,
    l: i64,
    m: i64,
    n: i64,
    o: i64,
    p: i64,
    q: i64,
    r: i64,
    s: i64,
    t: i64,
    u: i64,
    v: i64,
    w: i64,
    x: i64,
    y: i64,
    z: i64,
}

#[rustler::nif(name = "nifrecord_benchmark")]
pub fn benchmark(my_struct: MyRecord, op: Operation) -> Option<MyRecord> {
    match op {
        Operation::Decode => None,
        Operation::DecodeAndEncode => Some(my_struct),
    }
}
