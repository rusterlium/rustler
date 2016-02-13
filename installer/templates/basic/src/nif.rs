#![feature(plugin)]
#![plugin(rustler_codegen)]

#[macro_use]
extern crate rustler;
use rustler::{ NifEnv, NifTerm, NifResult, NifDecoder, NifEncoder };

rustler_export_nifs!(
    "<%= native_module %>",
    [("add", 2, add)],
    None
);

fn add<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let num1: i64 = try!(NifDecoder::decode(args[0]));
    let num2: i64 = try!(NifDecoder::decode(args[1]));
    Ok((num1 + num2).encode(env))
}
