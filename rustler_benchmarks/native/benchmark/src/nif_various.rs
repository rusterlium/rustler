use rustler::{atoms, Atom, NifResult, Term};
use rustler::{NifStruct, NifTaggedEnum};
use std::collections::HashMap;

atoms! {
    test_a,
    test_b,
    test
}

#[derive(NifStruct)]
#[module = "Benchmark.NifVarious.TestStructString"]
pub struct TestStructString {
    a: String,
}

#[derive(NifStruct)]
#[module = "Benchmark.NifVarious.TestStruct"]
pub struct TestStruct {
    a: String,
    b: i64,
    c: Option<String>,
    d: bool,
}

#[derive(NifTaggedEnum)]
pub enum TaggedEnum {
    UnitA,
    UnitB,
    UnitC,
    UnitD,
    UnitE,
    UnitF,
    GenericA(String),
    GenericB(String),
    GenericC(String),
    GenericD(String),
    GenericE(String),
    GenericF(String),
    GenericG(String),
    GenericH(String),
    GenericI(String),
    GenericJ(String),
    Tuple((String, i64)),
    List(Vec<String>),
    Map(HashMap<i64, String>),
    String(String),
    Int(i32),
    TestStruct(TestStruct),
}

#[rustler::nif]
pub fn decode_term(input: Term) -> NifResult<bool> {
    Ok(!input.is_atom())
}

#[rustler::nif]
pub fn decode_string(input: String) -> NifResult<bool> {
    Ok(!input.is_empty())
}

#[rustler::nif]
pub fn decode_struct_string(input: TestStructString) -> NifResult<bool> {
    Ok(!input.a.is_empty())
}

#[rustler::nif]
pub fn decode_struct(input: TestStruct) -> NifResult<bool> {
    Ok(input.d)
}

#[rustler::nif]
pub fn decode_tagged_enum(input: TaggedEnum) -> NifResult<bool> {
    match input {
        TaggedEnum::UnitA => Ok(true),
        _ => Ok(false),
    }
}

#[rustler::nif]
pub fn encode_tagged_enum() -> TaggedEnum {
    TaggedEnum::TestStruct(TestStruct {
        a: "abc".to_string(),
        b: 124,
        c: None,
        d: true,
    })
}

#[rustler::nif]
pub fn void() {}

#[rustler::nif]
pub fn encode_atom() -> Atom {
    test_a()
}

#[rustler::nif]
pub fn compare_atom(a: Atom) -> Atom {
    if a == test_a() {
        return test_a();
    }
    if a == test_a() {
        return test_a();
    }
    if a == test_a() {
        return test_a();
    }
    a
}
