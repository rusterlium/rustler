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
