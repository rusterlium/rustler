extern crate rustler_sys;

#[cfg(unix)]
#[test]
fn test1() {
    use rustler_sys::*;
    use std::collections::HashMap;
    use std::env;
    use std::iter::FromIterator;
    use std::mem::size_of;
    use std::path::Path;
    use std::process::Command;

    let out_dir = env::var("OUT_DIR").unwrap();

    // use environment escript and cc if available
    let escript = env::var("ESCRIPT").unwrap_or_else(|_| "escript".to_string());
    let cc = env::var("CC").unwrap_or_else(|_| "cc".to_string());

    // get erts include path
    let erts_include = Command::new(escript)
        .arg("get_erts_path.erl")
        .output()
        .map_err(|_| "Can't run escript")
        .map(|out| {
            if out.status.success() {
                out.stdout
            } else {
                panic!("Can't run get_erts_path.erl")
            }
        })
        .map(String::from_utf8)
        .unwrap()
        .unwrap();

    //println!("include {:?}", erts_include);

    // Compile C program
    let exe = Path::new(&out_dir).join("struct_size");
    if !Command::new(cc)
        .arg("-o")
        .arg(&exe)
        .arg("-I")
        .arg(&erts_include)
        .arg("tests/struct_size.c")
        .status()
        .map_err(|_| "Can't find c compiler (cc or value of environment CC)")
        .unwrap()
        .success()
    {
        panic!("Can't compile struct_size.c")
    }

    // Run C program that lists C NIF runtime information.
    let stdout: Vec<u8> = Command::new(&exe)
        .output()
        .map_err(|_| "Can't run C runtime information program")
        .unwrap()
        .stdout;

    let output: &str = std::str::from_utf8(&stdout).unwrap();

    // Parse C program output into hashmap of (&str, u32)
    let sizemap = HashMap::<&str, usize>::from_iter(
        output
            .lines()
            .map(|ln| ln.split(' '))
            .map(|mut it| (it.next().unwrap(), it.next().unwrap().parse().unwrap())),
    );

    /* types to check are:

    ERL_NIF_UINT
    ERL_NIF_TERM
    ErlNifFunc
    ErlNifEntry
    ErlNifBinary
    ErlNifPid
    ErlNifSysInfo
    ErlNifMapIterator

    */

    assert_eq!(
        &size_of::<ERL_NIF_UINT>(),
        sizemap.get("ERL_NIF_UINT").unwrap()
    );
    assert_eq!(
        &size_of::<ERL_NIF_TERM>(),
        sizemap.get("ERL_NIF_TERM").unwrap()
    );
    assert_eq!(&size_of::<ErlNifFunc>(), sizemap.get("ErlNifFunc").unwrap());

    // Disabling this test because struct size grew in otp-20 but remains
    // backwards compatible.

    //assert_eq!(
    //&size_of::<ErlNifEntry>(),
    //sizemap.get("ErlNifEntry").unwrap()
    //);

    //assert_eq!(
    //&size_of::<ErlNifBinary>(),
    //sizemap.get("ErlNifBinary").unwrap()
    //);
    assert_eq!(&size_of::<ErlNifPid>(), sizemap.get("ErlNifPid").unwrap());
    assert_eq!(
        &size_of::<ErlNifSysInfo>(),
        sizemap.get("ErlNifSysInfo").unwrap()
    );
    assert_eq!(
        &size_of::<ErlNifMapIterator>(),
        sizemap.get("ErlNifMapIterator").unwrap()
    );
}
