use rustler::{Env, Term, Encoder, NifResult};

#[derive(NifTuple)]
struct AddTuple {
    lhs: i32,
    rhs: i32,
}

pub fn tuple_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let tuple: AddTuple = args[0].decode()?;
    Ok(tuple.encode(env))
}

#[derive(NifRecord)]
#[tag = "record"]
struct AddRecord {
    lhs: i32,
    rhs: i32,
}

pub fn record_echo<'a>(env: NifEnv<'a>, args: &[NifTerm<'a>]) -> NifResult<NifTerm<'a>> {
    let record: AddRecord = args[0].decode()?;
    Ok(record.encode(env))
}

#[derive(NifMap)]
struct AddMap {
    lhs: i32,
    rhs: i32,
}

pub fn map_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let map: AddMap = try!(args[0].decode());
    Ok(map.encode(env))
}

#[derive(NifStruct)]
#[module = "AddStruct"]
struct AddStruct {
    lhs: i32,
    rhs: i32,
}

pub fn struct_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let add_struct: AddStruct = args[0].decode()?;
    Ok(add_struct.encode(env))
}

#[derive(NifUnitEnum)]
enum UnitEnum {
    FooBar,
    Baz,
}

pub fn unit_enum_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let unit_enum: UnitEnum = args[0].decode()?;
    Ok(unit_enum.encode(env))
}

#[derive(NifUntaggedEnum)]
enum UntaggedEnum {
    Foo(u32),
    Bar(String),
    Baz(AddStruct),
}

pub fn untagged_enum_echo<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let untagged_enum: UntaggedEnum = args[0].decode()?;
    Ok(untagged_enum.encode(env))
}
