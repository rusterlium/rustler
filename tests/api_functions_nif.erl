-module(api_functions_nif).
-export([start/0, test_enif_make_int/0, test_enif_make_pid/0]).

load_nif() ->
    Lib = filename:join(os:getenv("OUT_DIR"), "api_functions_nif"),
    erlang:load_nif(Lib, []).

test_enif_make_int() ->
    "NIF library not loaded".
test_enif_make_pid() ->
    "NIF library not loaded".

start() ->
    io:fwrite("nif_loaded ~w~n", [load_nif() == ok]),
    io:fwrite("enif_make_int ~w~n", [test_enif_make_int() == -10]),
    io:fwrite("enif_make_pid ~w~n", [is_pid(test_enif_make_pid())]).
