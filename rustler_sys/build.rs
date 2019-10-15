// build.rs
//
// Execute Erlang script to generate API lists and extract config.
//

use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    let escript = env::var("ESCRIPT").unwrap_or("escript".to_string());

    let target_pointer_width = match env::var("CARGO_CFG_TARGET_POINTER_WIDTH") {
        Ok(ref val) if val == "32" => "4",
        Ok(ref val) if val == "64" => "8",
        Ok(ref val) => panic!("Unsupported target pointer width: {}", val),
        Err(err) => panic!(
            "An error occurred while determining the pointer width to compile `rustler_sys` for:\n\n{:?}\n\nPlease report a bug.",
            err
        ),
    };

    // setup output directory
    let out_dir = env::var("OUT_DIR")
        .map_err(|_| "Can't read OUT_DIR env variable.")
        .unwrap();

    let dst = Path::new(&out_dir);
    match Command::new(escript)
        .arg("gen_api.erl")
        .arg(target_pointer_width.to_string())
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
