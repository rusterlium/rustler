
extern crate ruster_unsafe;
use ruster_unsafe::*;

use std::process::Command;
use std::env;
use std::path::Path;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::mem::size_of;



#[test]
fn test1() {
	let out_dir = env::var("OUT_DIR").unwrap();

	// get erts include path
	let erts_include = String::from_utf8(Command::new("escript").arg("get_erts_path.erl")
		.output().unwrap().stdout).unwrap();

	println!("include {:?}", erts_include)
;
	// Compile C program
	let exe = Path::new(&out_dir).join("struct_size"); 
    Command::new("cc").arg("-o").arg(&exe).arg("-I").arg(&erts_include).arg("tests/struct_size.c")
    	.status().unwrap();

    // Run C program that lists C NIF runtime information.
    let stdout:Vec<u8> = Command::new(&exe).output().unwrap().stdout;
 	let output:&str = std::str::from_utf8(&stdout).unwrap();

 	// Parse C program output into hashmap of (&str, u32)
	let sizemap = HashMap::<&str, usize>::from_iter(
		output.lines().map(|ln|ln.split(" "))
		.map(|mut it| (it.next().unwrap(), it.next().unwrap().parse().unwrap())));



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

	assert_eq!(&size_of::<ERL_NIF_UINT>(),      sizemap.get("ERL_NIF_UINT").unwrap());          
	assert_eq!(&size_of::<ERL_NIF_TERM>(),      sizemap.get("ERL_NIF_TERM").unwrap());          
	assert_eq!(&size_of::<ErlNifFunc>(),        sizemap.get("ErlNifFunc").unwrap());        
	assert_eq!(&size_of::<ErlNifEntry>(),       sizemap.get("ErlNifEntry").unwrap());         
	assert_eq!(&size_of::<ErlNifBinary>(),      sizemap.get("ErlNifBinary").unwrap());          
	assert_eq!(&size_of::<ErlNifPid>(),         sizemap.get("ErlNifPid").unwrap());       
	assert_eq!(&size_of::<ErlNifSysInfo>(),     sizemap.get("ErlNifSysInfo").unwrap());           
	assert_eq!(&size_of::<ErlNifMapIterator>(), sizemap.get("ErlNifMapIterator").unwrap());               
}
