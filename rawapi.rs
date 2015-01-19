

use erlnif::conf;

struct ERL_NIF_TERM {
	data: conf::erl_term,
}

struct ErlNifEnv;


struct ErlNifBinary {
	size: usize,
	data: *mut [u8],
	// (additional internal members elided.)
}

struct ErlNifResourceType;

//typedef void ErlNifResourceDtor(ErlNifEnv*, void*);

/*
typedef enum
{
ERL_NIF_RT_CREATE = 1,
ERL_NIF_RT_TAKEOVER = 2
}ErlNifResourceFlags;
typedef enum
{
ERL_NIF_LATIN1 = 1
}ErlNifCharEncoding;
typedef struct
{
ERL_NIF_TERM pid; /* internal, may change */
}ErlNifPid;
typedef ErlDrvSysInfo ErlNifSysInfo;
*/

extern {
	fn enif_get_int( env: *mut ErlNifEnv, term: ERL_NIF_TERM, ip: *mut conf::Int) -> int;
	fn enif_make_int( env: *mut ErlNifEnv, i: conf::Int) -> ERL_NIF_TERM;
}


