// build.rs
//
// Execute Erlang script to generate API lists and extract config.
//

use std::io::Command;
use std::os;

fn main() {
	// FIXME: take env escript if present
	let escript = "escript";

	// emit generated files to OUT_DIR
	let dst = Path::new(os::getenv("OUT_DIR").unwrap());
	Command::new(escript).arg("gen_api.erl").arg(dst).status().unwrap();
}
