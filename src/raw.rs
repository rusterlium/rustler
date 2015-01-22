use libc::c_int;
use libc::c_uint;
use libc::c_char;
use libc::c_uchar;
use libc::c_void;
use libc::size_t;
use libc::c_ulong;
use libc::c_long;
use libc::c_double;
use std::option::Option;



include!(concat!(env!("OUT_DIR"), "/nif_versions.snippet"));
// example of included content:
// const NIF_MAJOR_VERSION: c_int = 2;
// const NIF_MINOR_VERSION: c_int = 7;


// FIXME
#[allow(non_camel_case_types)]
type ERL_NIF_UINT = size_t;
//type ERL_NIF_UINT = usize;  // users complain about non-ffi type.

#[repr(C)]
struct ERL_NIF_TERM(ERL_NIF_UINT);

#[repr(C)]
struct ErlNifEnv;

#[repr(C)]
pub struct ErlNifFunc {
	name: *const c_char,
	arity: c_uint,
	function: extern "C" fn(env: *mut ErlNifEnv, argc: c_int, argv: *const ERL_NIF_TERM) -> ERL_NIF_TERM,
	flags: c_uint,
}

#[repr(C)]
struct ErlNifEntry {
	major:        c_int,
	minor:        c_int,
	name:         *const str,
	num_of_funcs: c_int,
	funcs:        *mut ErlNifFunc,
	load:    Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM)-> c_int>,
	reload:  Option<extern "C" fn(arg1: *mut ErlNifEnv, priv_data: *mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
	upgrade: Option<extern "C" fn(arg1: *mut ErlNifEnv,	priv_data: *mut *mut c_void, old_priv_data:	*mut *mut c_void, load_info: ERL_NIF_TERM) -> c_int>,
	unload:  Option<extern "C" fn(arg1: *mut ErlNifEnv,	priv_data: *mut c_void)	-> ()>,
	vm_variant: *const str,
}

#[repr(C)]
pub struct ErlNifBinary {
	size: size_t,
	data: *mut [u8],
	/* Internals (avert your eyes) */
	bin_term: ERL_NIF_TERM,
	ref_bin: *mut c_void,
}

#[repr(C)]
pub struct ErlNifResourceType;

pub type ErlNifResourceDtor = extern "C" fn(arg1: *mut ErlNifEnv, arg2: *mut c_void) -> ();

#[repr(C)]
pub enum ErlNifResourceFlags {
	ERL_NIF_RT_CREATE = 1,
	ERL_NIF_RT_TAKEOVER = 2,
}

#[repr(C)]
pub enum ErlNifCharEncoding {
	ERL_NIF_LATIN1 = 1,
	DUMMY = 999, // prevents "univariant enum" compile error
}

#[repr(C)]
pub struct ErlNifPid {
	pid: ERL_NIF_TERM,
}

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

#[repr(C)]
pub enum ErlNifDirtyTaskFlags {
	ERL_NIF_DIRTY_JOB_CPU_BOUND = 1,
	ERL_NIF_DIRTY_JOB_IO_BOUND = 2,
}

#[repr(C)]
pub struct ErlNifMapIterator {
	map: ERL_NIF_TERM,
	t_limit: ERL_NIF_UINT,
	idx: ERL_NIF_UINT,
	ks: *mut ERL_NIF_TERM,
	vs: *mut ERL_NIF_TERM,
	__spare__: [*mut c_void; 2us],
}

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