
use erlnif::rawapi;

use erlnif::conf::Int;
use erlnif::conf::Uint;

struct ErlNifEntry {
	major: Int,
	minor: Int,
	name: *const str,
	num_of_funcs: Int,
ErlNifFunc* funcs;
int (*load) (ErlNifEnv*, void** priv_data, ERL_NIF_TERM load_info);
int (*reload) (ErlNifEnv*, void** priv_data, ERL_NIF_TERM load_info);
int (*upgrade)(ErlNifEnv*, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
void (*unload) (ErlNifEnv*, void* priv_data);

	vm_variant: *const str,
	options: Uint
}



#[no_mangle]
pub extern fn nif_init() *mut ErlNifEntry

#define ERL_NIF_INIT(NAME, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD) \
ERL_NIF_INIT_PROLOGUE \
ERL_NIF_INIT_GLOB \
ERL_NIF_INIT_DECL(NAME); \
ERL_NIF_INIT_DECL(NAME) \
{ \
static ErlNifEntry entry = \
{ \
ERL_NIF_MAJOR_VERSION, \
ERL_NIF_MINOR_VERSION, \
#NAME, \
sizeof(FUNCS) / sizeof(*FUNCS), \
FUNCS, \
LOAD, RELOAD, UPGRADE, UNLOAD, \
ERL_NIF_VM_VARIANT, \
ERL_NIF_ENTRY_OPTIONS \
}; \
ERL_NIF_INIT_BODY; \
return &entry; \
} \
ERL_NIF_INIT_EPILOGUE
#if defined(USE_DYNAMIC_TRACE) && (defined(USE_DTRACE) || defined(USE_SYSTEMTAP))
#define HAVE_USE_DTRACE 1
#endif
