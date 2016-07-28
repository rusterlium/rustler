#![feature(link_args)]
#![feature(plugin)]
// Rustler compiler plugin
#![plugin(rustler_codegen)]
// Necessary hack for building on Mac
#[cfg(target_os="macos")]
#[link_args = "-flat_namespace -undefined suppress"]
extern {}

#[macro_use]
extern crate rustler;
use rustler::{ NifEnv, NifTerm, NifResult, NifEncoder };

rustler_export_nifs!(
    "<%= native_module %>",
    [("add", 2, add)],
    None
);

fn add<'a>(env: &'a NifEnv, args: &Vec<NifTerm>) -> NifResult<NifTerm<'a>> {
    let num1: i64 = try!(args[0].decode());
    let num2: i64 = try!(args[1].decode());
    Ok((num1 + num2).encode(env))
}
