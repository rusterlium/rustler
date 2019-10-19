use rustler::types::truthy::Truthy;
use rustler::{Encoder, Env, NifResult, Term};
use rustler::{NifMap, NifRecord, NifStruct, NifTuple, NifUnitEnum, NifUntaggedEnum};

#[derive(NifTuple)]
pub struct AddTuple {
    lhs: i32,
    rhs: i32,
}

pub fn tuple_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<AddTuple> {
    let tuple: AddTuple = args[0].decode()?;
    Ok(tuple)
}

#[derive(NifRecord)]
#[rustler(encode, decode)] // Added to check encode/decode attribute, #180
#[must_use] // Added to check attribute order (see similar issue #152)
#[tag = "record"]
pub struct AddRecord {
    lhs: i32,
    rhs: i32,
}

pub fn record_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<AddRecord> {
    let record: AddRecord = args[0].decode()?;
    Ok(record)
}

#[derive(NifMap)]
pub struct AddMap {
    lhs: i32,
    rhs: i32,
}

pub fn map_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<AddMap> {
    let map: AddMap = args[0].decode()?;
    Ok(map)
}

#[derive(Debug, NifStruct)]
#[must_use] // Added to test Issue #152
#[module = "AddStruct"]
pub struct AddStruct {
    lhs: i32,
    rhs: i32,
}

pub fn struct_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<AddStruct> {
    let add_struct: AddStruct = args[0].decode()?;
    Ok(add_struct)
}

#[derive(NifUnitEnum)]
pub enum UnitEnum {
    FooBar,
    Baz,
}

pub fn unit_enum_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<UnitEnum> {
    let unit_enum: UnitEnum = args[0].decode()?;
    Ok(unit_enum)
}

#[derive(NifUntaggedEnum)]
pub enum UntaggedEnum {
    Foo(u32),
    Bar(String),
    Baz(AddStruct),
    Bool(bool),
}

pub fn untagged_enum_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<UntaggedEnum> {
    let untagged_enum: UntaggedEnum = args[0].decode()?;
    Ok(untagged_enum)
}

#[derive(NifUntaggedEnum)]
pub enum UntaggedEnumWithTruthy {
    Baz(AddStruct),
    Truthy(Truthy),
}

pub fn untagged_enum_with_truthy<'a>(
    _env: Env<'a>,
    args: &[Term<'a>],
) -> NifResult<UntaggedEnumWithTruthy> {
    let untagged_enum: UntaggedEnumWithTruthy = args[0].decode()?;
    Ok(untagged_enum)
}

#[derive(NifTuple)]
pub struct Newtype(i64);

pub fn newtype_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<Newtype> {
    let newtype: Newtype = args[0].decode()?;
    Ok(newtype)
}

#[derive(NifTuple)]
pub struct TupleStruct(i64, i64, i64);

pub fn tuplestruct_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<TupleStruct> {
    let tuplestruct: TupleStruct = args[0].decode()?;
    Ok(tuplestruct)
}

#[derive(NifRecord)]
#[tag = "newtype"]
pub struct NewtypeRecord(i64);

pub fn newtype_record_echo<'a>(_env: Env<'a>, args: &[Term<'a>]) -> NifResult<NewtypeRecord> {
    let newtype: NewtypeRecord = args[0].decode()?;
    Ok(newtype)
}

#[derive(NifRecord)]
#[tag = "tuplestruct"]
pub struct TupleStructRecord(i64, i64, i64);

pub fn tuplestruct_record_echo<'a>(
    _env: Env<'a>,
    args: &[Term<'a>],
) -> NifResult<TupleStructRecord> {
    let tuplestruct: TupleStructRecord = args[0].decode()?;
    Ok(tuplestruct)
}
