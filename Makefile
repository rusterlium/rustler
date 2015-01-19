

.PHONY: all

all: liberlnif.rlib

env.mk:
	$(gen_verbose) erl -noshell -noinput -eval "file:write_file(\"$@\", \
		io_lib:format(\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/\", \
			[code:root_dir(), erlang:system_info(version)])), \
		init:stop()."

include env.mk

nifconf_gen.o: nifconf_gen.c env.mk
	$(CC) -c -I $(ERTS_INCLUDE_DIR) -o $@ $<

nifconf_gen: nifconf_gen.o

conf.rs: nifconf_gen
	./nifconf_gen >$@

liberlnif.rlib: erlnif.rs conf.rs
	rustc $<
