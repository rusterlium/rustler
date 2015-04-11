// build.rs
//
// Execute Erlang script to generate API lists and extract config.
//

use std::process::Command;
use std::env;
use std::path::Path;

fn main() {
	// FIXME: take env escript if present
	let escript = "escript";

	// emit generated files to OUT_DIR
	let out_dir = env::var("OUT_DIR").unwrap();
	let dst = Path::new(&out_dir);
	Command::new(escript).arg("gen_api.erl").arg(dst).status().unwrap();
}
