#!/usr/bin/escript

main(_) ->
	io:format("~s/erts-~s/include/",
			[code:root_dir(),
			 erlang:system_info(version)]).
