
pub use libc::c_int;
use libc::c_uint;
use libc::c_char;
use libc::c_uchar;
use libc::c_void;
use libc::size_t;
use libc::c_ulong;
use libc::c_long;
use libc::c_double;
use std::option::Option;
//use core::marker::Sync;

include!(concat!(env!("OUT_DIR"), "/nif_versions.snippet"));
// example of included content:
// const NIF_MAJOR_VERSION: c_int = 2;
// const NIF_MINOR_VERSION: c_int = 7;


// FIXME
#[allow(non_camel_case_types)]
pub type ERL_NIF_UINT = size_t;
//type ERL_NIF_UINT = usize;  // users complain about non-ffi type.


#[allow(non_camel_case_types)]
pub type ERL_NIF_TERM = *const c_void;
//pub type ERL_NIF_TERM = ERL_NIF_UINT;

// LLVM doesn't like to return structs for extern functions, so the following doesn't work.
// #[derive(Copy)]
// #[repr(C)]
// pub struct ERL_NIF_TERM<'a> {
// 	ptr: *mut c_void, // Dummy pointer.  The purpose is to take up just the right amount of space.
// 	marker: ContravariantLifetime<'a>,
// }


#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifEnv;

// #[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifFunc {
	pub name:     *const u8,
	pub arity:    c_uint,
//	pub function: Option<extern "C" fn(env: *mut ErlNifEnv, argc: c_int, argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM>,
	pub function: extern "C" fn(env: *mut ErlNifEnv, argc: c_int, argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM,
	pub flags:    c_uint,
}
// unsafe impl Sync for ErlNifFunc {}


// #[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifEntry {
	pub major:        c_int,
	pub minor:        c_int,
	pub name:         *const u8,
	pub num_of_funcs: c_int,
	pub funcs:        *const ErlNifFunc,
	pub load:    Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int>,
	pub reload:  Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
	pub upgrade: Option<extern "C" fn(arg1: *mut ErlNifEnv,	priv_data: *mut *mut c_void, old_priv_data:	*mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
	pub unload:  Option<extern "C" fn(arg1: *mut ErlNifEnv,	priv_data: *mut c_void)	-> ()>,
	pub vm_variant: *const u8,
	pub options: c_uint,
}
//unsafe impl Sync for ErlNifEntry {}

#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifBinary {
	size: size_t,
	data: *mut [u8],
	/* Internals (avert your eyes) */
	bin_term: ERL_NIF_TERM,
	ref_bin: *mut c_void,
}

#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifResourceType;

#[allow(missing_copy_implementations)]
pub type ErlNifResourceDtor = extern "C" fn(arg1: *mut ErlNifEnv, arg2: *mut c_void) -> ();

#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifResourceFlags {
	ERL_NIF_RT_CREATE = 1,
	ERL_NIF_RT_TAKEOVER = 2,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifCharEncoding {
	ERL_NIF_LATIN1 = 1,
	DUMMY = 999, // prevents "univariant enum" compile error
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct ErlNifPid {
	pid: ERL_NIF_TERM,
}

#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifSysInfo {
	pub driver_major_version: c_int,
	pub driver_minor_version: c_int,
	pub erts_version: *mut c_char,
	pub otp_release: *mut c_char,
	pub thread_support: c_int,
	pub smp_support: c_int,
	pub async_threads: c_int,
	pub scheduler_threads: c_int,
	pub nif_major_version: c_int,
	pub nif_minor_version: c_int,
	pub dirty_scheduler_support: c_int,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifDirtyTaskFlags {
	ERL_NIF_DIRTY_JOB_CPU_BOUND = 1,
	ERL_NIF_DIRTY_JOB_IO_BOUND = 2,
}

#[allow(missing_copy_implementations)]
#[repr(C)]
pub struct ErlNifMapIterator {
	map: ERL_NIF_TERM,
	t_limit: ERL_NIF_UINT,
	idx: ERL_NIF_UINT,
	ks: *mut ERL_NIF_TERM,
	vs: *mut ERL_NIF_TERM,
	__spare__: [*mut c_void; 2us],
}

#[derive(Copy, Clone)]
#[repr(C)]
pub enum ErlNifMapIteratorEntry {
	ERL_NIF_MAP_ITERATOR_HEAD = 1,
	ERL_NIF_MAP_ITERATOR_TAIL = 2,
}




include!(concat!(env!("OUT_DIR"), "/nif_api.snippet"));
// example of included content:
// extern "C" {
//     pub fn enif_priv_data(arg1: *mut ErlNifEnv) -> *mut c_void;
//     pub fn enif_alloc(size: size_t) -> *mut c_void;
//     pub fn enif_free(ptr: *mut c_void);
//     pub fn enif_is_atom(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
//     pub fn enif_is_binary(arg1: *mut ErlNifEnv, term: ERL_NIF_TERM) -> c_int;
// ...

#[cfg(target_pointer_width = "64")]
pub fn enif_make_int64(env: *mut ErlNifEnv, i: i64) -> ERL_NIF_TERM
 	{ unsafe {enif_make_long(env, i)}}

#[cfg(target_pointer_width = "64")]
pub fn enif_make_uint64(env: *mut ErlNifEnv, i: u64) -> ERL_NIF_TERM
 	{ unsafe {enif_make_ulong(env, i) }}

#[cfg(target_pointer_width = "64")]
pub fn enif_get_int64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut i64) -> c_int
 	{ unsafe {enif_get_long(env, term, ip) }}

#[cfg(target_pointer_width = "64")]
pub fn enif_get_uint64(env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut u64) -> c_int
 	{ unsafe {enif_get_ulong(env, term, ip) }}

/*

#[no_mangle]
pub extern "C" fn nif_init() -> *mut ErlNifEntry
{
	static mut entry: ErlNifEntry = ErlNifEntry{
		major : NIF_MAJOR_VERSION,
		minor : NIF_MINOR_VERSION,
		name : "rustnif\0",
		num_of_funcs : 0,
		funcs : 0 as *mut ErlNifFunc,
		load :    None,
		reload :  None,
		upgrade : None,
		unload :  None,
		vm_variant : "beam.vanilla\0",
	};
	// unsafe {
	// 	funcs[0].name = CString::from_slice("native_add".as_bytes()).as_ptr();
	// 	entry.name = CString::from_slice("er".as_bytes()).as_ptr();
	// 	entry.num_of_funcs = funcs.len() as i32;
	// 	entry.funcs = funcs.as_mut_ptr();
	
	// }
	unsafe{ &mut entry }
}

*/