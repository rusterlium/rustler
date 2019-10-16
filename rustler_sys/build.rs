// build.rs
//
// Execute Erlang script to generate API lists and extract config.
//

use std::{env, fs};
use std::path::Path;
use std::process::Command;

const SNIPPET_NAME: &str = "nif_api.snippet";

fn try_gen_api(dst: &Path, pointer_size: usize) -> bool {
    // use environment escript if available
    let escript = env::var("ESCRIPT").unwrap_or("escript".to_string());

    if let Ok(res) = Command::new(escript)
        .arg("gen_api.erl")
        .arg(pointer_size.to_string())
        .arg(dst)
        .status()
        .map_err(|_| "Failed to start gen_api.erl.  Is 'escript' available in the path?")
    {
        res.success()
    } else {
        false
    }
}

fn main() {
    // get size of C pointer
    let target_pointer_width = match env::var("CARGO_CFG_TARGET_POINTER_WIDTH") {
        Ok(ref val) if val == "32" => 4,
        Ok(ref val) if val == "64" => 8,
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

    let dst = Path::new(&out_dir).join(SNIPPET_NAME);

    if !try_gen_api(&dst, target_pointer_width) {
        eprintln!("Failed to generate API from local installation, falling back to precompiled");

        let source = Path::new(&"precompiled").join(format!("nif_api.{}.snippet", target_pointer_width));

        fs::copy(&source, &dst).unwrap();
    }
}
