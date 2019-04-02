extern crate cargo_erlangapp;
extern crate itertools;
extern crate walkdir;

//use cargo_erlangapp::{Target, target_filenames};
//use std::ffi::{OsStr};
use itertools::Itertools;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fs, io};
use walkdir::WalkDir;

#[test]
fn do_test() {
    // get working directory where we can create files
    let out_dir = env::var("OUT_DIR").unwrap();

    // find the directory for the Erlang test application
    let mut appdir_src = PathBuf::new();
    appdir_src.push("tests");
    appdir_src.push("testapp");

    // copy application to working dir
    copy_all(&appdir_src, &out_dir).unwrap();

    // get working dir for Erlang test application
    let mut appdir = PathBuf::new();
    appdir.push(&out_dir);
    appdir.push("testapp");

    let escript = env::var("ESCRIPT").unwrap_or("escript".to_string());

    // build test crate
    invoke_erlangapp(&["cargo-erlangapp", "build"], &appdir);

    // compile and run erlang eunit tests
    match Command::new(escript)
        .current_dir(&appdir)
        .arg("dotest.escript")
        .status()
        .expect("failed to execute escript")
        .success()
    {
        true => (),
        false => panic!("erlang tests failed"),
    };
}

fn invoke_erlangapp(args: &[&str], path: &Path) {
    cargo_erlangapp::invoke_with_args_str(args, path)
}

fn copy_all<P: AsRef<Path>, Q: AsRef<Path>>(from: P, to: Q) -> io::Result<()> {
    // calculate how many path elements to chop off entry when forming to path
    let chop_cnt = from.as_ref().components().count() - 1;
    for entry in WalkDir::new(from).follow_links(false) {
        let entry = try!(entry);
        let filetype = entry.file_type();
        let compi = entry.path().components().dropping(chop_cnt);
        let to_path = to.as_ref().join(compi.as_path());
        //let to_path = to.as_ref().join(entry.path());
        if filetype.is_dir() {
            try!(fs::create_dir_all(to_path));
        } else if filetype.is_file() {
            try!(fs::copy(entry.path(), to_path));
        }
    }
    Ok(())
}
