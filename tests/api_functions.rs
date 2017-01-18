use std::process::Command;
use std::env;
use std::path::Path;
use std::collections::HashMap;
use std::iter::FromIterator;

#[cfg(unix)]
#[test]
fn test_api_functions() {
	let out_dir = env::var("OUT_DIR").unwrap();

	// Use environment erl, erlc and rustc if available
	let erl     = env::var("ERL").unwrap_or("erl".to_string());
	let erlc    = env::var("ERLC").unwrap_or("erlc".to_string());
	let rustc   = env::var("RUSTC").unwrap_or("rustc".to_string());

	// Compile rust nif library
    let path = Path::new(&out_dir).join("../../../deps");
    // TODO: .so extension is platform-dependent - fix it
    let out = Path::new(&out_dir).join("api_functions_nif.so");
    match Command::new(rustc).arg("-L").arg(&path).arg("--crate-type").arg("dylib")
                             .arg("-o").arg(&out).arg("tests/api_functions_nif.rs")
    	.status()
    	.map_err(|_|"Can't find rust compiler (rustc or value of environment RUSTC)")
    	.unwrap().success() {
    		false => panic!("Can't compile api_functions_nif.rs"),
    		_     => ()
    	};

	// Compile erlang nif library
    match Command::new(erlc).arg("-o").arg(&out_dir).arg("tests/api_functions_nif.erl")
    	.status()
    	.map_err(|_|"Can't find erlang compiler (erlc or value of environment ERLC)")
    	.unwrap().success() {
    		false => panic!("Can't compile api_functions_nif.erl"),
    		_ => ()
    	};

    // Execute erlang tests
    let stdout:Vec<u8> = Command::new(erl).arg("-pz").arg(&out_dir).arg("-noshell")
                                          .arg("-s").arg("api_functions_nif")
                                          .arg("-s").arg("init").arg("stop").output()
    	.map_err(|_|"Can't run api_functions_nif.erl")
    	.unwrap().stdout;
 	let output:&str = std::str::from_utf8(&stdout).unwrap();

    //println!("\nerlang output:\n {}\n", output);

 	// Parse erl program output into hashmap of (&str, &str)
	let sizemap = HashMap::<&str, &str>::from_iter(
		output.lines().map(|ln|ln.split(" "))
		.map(|mut it| (it.next().unwrap(), it.next().unwrap())));

	assert_eq!(sizemap.get("nif_loaded").unwrap(), &"true");
	assert_eq!(sizemap.get("enif_make_int").unwrap(), &"true");
	assert_eq!(sizemap.get("enif_make_pid").unwrap(), &"true");
}
