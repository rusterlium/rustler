// build.rs
//
// Execute Erlang script to generate API lists and extract config.
//

use std::env;
use std::os::raw::c_ulong;
use std::path::Path;
use std::process::Command;

fn main() {
    // use environment escript if available
    let escript = env::var("ESCRIPT").unwrap_or("escript".to_string());

    // get size of C long
    let ulong_size = std::mem::size_of::<c_ulong>();

    // setup output directory
    let out_dir = env::var("OUT_DIR")
        .map_err(|_| "Can't read OUT_DIR env variable.")
        .unwrap();

    let dst = Path::new(&out_dir);
    match Command::new(escript)
        .arg("gen_api.erl")
        .arg(ulong_size.to_string())
        .arg(dst)
        .status()
        .map_err(|_| "Failed to start gen_api.erl.  Is 'escript' available in the path?")
        .unwrap()
        .success()
    {
        true => (),
        false => panic!("gen_api.erl encountered an error."),
    }
}
