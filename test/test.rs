#![feature(plugin)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
use rustler::{NifEnv, NifTerm, NifError, NifEncoder, NifResult};

rustler_export_nifs!(
    "Elixir.RustlerNative",
    [("add_u32", 2, add_u32),
     ("add_i32", 2, add_i32),
     ("tuple_add", 1, tuple_add)],
    None
);

fn add_u32<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let lhs: u32 = try!(args[0].decode());
    let rhs: u32 = try!(args[1].decode());

    Ok((lhs + rhs).encode(env))
}
fn add_i32<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let lhs: i32 = try!(args[0].decode());
    let rhs: i32 = try!(args[1].decode());

    Ok((lhs + rhs).encode(env))
}

#[NifTuple] struct AddTuple { lhs: i32, rhs: i32 }
fn tuple_add<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let tuple: AddTuple = try!(args[0].decode());
    Ok((tuple.lhs + tuple.rhs).encode(env))
}
