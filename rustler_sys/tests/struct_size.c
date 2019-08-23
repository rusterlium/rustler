#include <stdio.h>
#include <erl_nif.h>

int main() {
	printf("ERL_NIF_UINT %lu\n", sizeof(ERL_NIF_UINT));
	printf("ERL_NIF_TERM %lu\n", sizeof(ERL_NIF_TERM));
	printf("ErlNifFunc %lu\n", sizeof(ErlNifFunc));
	printf("ErlNifEntry %lu\n", sizeof(ErlNifEntry));
	printf("ErlNifBinary %lu\n", sizeof(ErlNifBinary));
	printf("ErlNifPid %lu\n", sizeof(ErlNifPid));
	printf("ErlNifSysInfo %lu\n", sizeof(ErlNifSysInfo));
	printf("ErlNifMapIterator %lu\n", sizeof(ErlNifMapIterator));

	return 0;
}
