-module(mynifmod).

-include_lib("eunit/include/eunit.hrl").


%%-export([init/0]).
-on_load(init/0).

init() ->
    {ok, Lib} = find_library("mynifmod", "mynifmod"),
    ok = erlang:load_nif(Lib, 0).

%% butchered version of https://github.com/goertzenator/find_crate/blob/master/src/find_crate.erl
find_library(CrateName, LibName) ->
	Wildcard = "priv/crates/" ++ CrateName ++ "/{lib,}" ++ LibName ++ ".{so,dll}",
	X = filelib:wildcard(Wildcard),
	case filelib:wildcard(Wildcard) of
		[Lib] -> {ok, filename:rootname(Lib)};
		[] -> {error, not_found};
		_ -> {error, multiple_matches}
	end.


simple_test_() -> [
		?_assertEqual(6, times2(3)),
		?_assertEqual(self(), test_enif_make_pid())
	].



times2(_X)           -> exit(nif_library_not_loaded).
test_enif_make_pid() -> exit(nif_library_not_loaded).




