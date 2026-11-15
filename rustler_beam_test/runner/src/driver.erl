%% The Erlang half of rustler_beam_test. A test binary starts one `erl` node
%% with this module. It loads the NIF library and runs each test NIF in a
%% process of its own.
%%
%% The test binary listens on a TCP port of the loopback interface, and the
%% node connects to it. Frames have a length prefix of 4 bytes:
%%
%%   to the node    <<Library/binary>>
%%   to the binary  <<0, Tests/binary>> | <<1, Reason/binary>>
%%   to the node    <<Id:32, Test/binary>>
%%   to the binary  <<Id:32, 0>> | <<Id:32, 1, Report/binary>>
%%
%% The first two load the library, and the other two run a test. `Tests` holds
%% <<Ignore:8, Size:16, Test:Size/binary>> for each test.
-module(rustler_beam_test_driver).
-export([main/1]).

main(Port) ->
    Options = [binary, {packet, 4}, {active, true}, {nodelay, true}],
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, Options),
    receive
        {tcp_closed, Socket} ->
            halt();
        {tcp, Socket, Library} ->
            try load(unicode:characters_to_list(Library)) of
                {Module, Tests} ->
                    ok = gen_tcp:send(Socket, [0 | Tests]),
                    serve(Socket, Module, #{})
            catch
                error:Reason ->
                    ok = gen_tcp:send(Socket, [1, format("~tp", [Reason])]),
                    halt(1)
            end
    end.

%% `erlang:load_nif/2` accepts a library only from the module that the library
%% names, and only when that module defines every NIF of the library. A library
%% that is built with `rustler_beam_test` writes both to the file that
%% RUSTLER_NIF_MANIFEST names when the BEAM asks it for them, which is before
%% the BEAM checks them. So one call that the BEAM refuses tells us the module
%% to compile, with a stub for each NIF.
load(Library) ->
    Manifest = os:getenv("RUSTLER_NIF_MANIFEST"),
    {error, Refusal} = erlang:load_nif(Library, 0),
    {Module, Functions} =
        case file:read_file(Manifest) of
            {ok, Lines} -> parse(binary:split(Lines, <<"\n">>, [global, trim]));
            {error, enoent} -> error({no_manifest, Refusal})
        end,
    Load = call(load_nif, [{string, 1, Library}, {integer, 1, 0}]),
    Forms = [
        {attribute, 1, module, Module},
        {attribute, 1, export, [{'$load', 0} | Functions]},
        {function, 1, '$load', 0, [{clause, 1, [], [], [Load]}]}
        | [stub(Function) || Function <- Functions]
    ],
    {ok, Module, Code} = compile:forms(Forms),
    {module, Module} = code:load_binary(Module, "rustler_beam_test", Code),
    ok = Module:'$load'(),
    ok = file:delete(Manifest),
    {Module, [describe(Module, Test, atom_to_binary(Test)) || {Test, 1} <- Functions]}.

%% A test is a NIF with this prefix in its name, and it tells whether it is
%% ignored.
describe(Module, Test, <<"rustler_beam_test ", _/binary>> = Name) ->
    Ignore =
        case Module:Test(describe) of
            true -> 1;
            false -> 0
        end,
    <<Ignore:8, (byte_size(Name)):16, Name/binary>>;
describe(_, _, _) ->
    <<>>.

parse([Module | Nifs]) ->
    {binary_to_atom(Module), [nif(binary:split(Nif, <<" ">>)) || Nif <- Nifs]}.

nif([Arity, Name]) ->
    {binary_to_atom(Name), binary_to_integer(Arity)}.

stub({Name, Arity}) ->
    Arguments = lists:duplicate(Arity, {var, 1, '_'}),
    NotLoaded = call(nif_error, [{atom, 1, not_loaded}]),
    {function, 1, Name, Arity, [{clause, 1, Arguments, [], [NotLoaded]}]}.

call(Function, Arguments) ->
    {call, 1, {remote, 1, {atom, 1, erlang}, {atom, 1, Function}}, Arguments}.

serve(Socket, Module, Running) ->
    receive
        {tcp, Socket, <<Id:32, Name/binary>>} ->
            Test = binary_to_existing_atom(Name),
            {_, Monitor} = spawn_monitor(fun() -> exit({verdict, run(Module, Test, start)}) end),
            serve(Socket, Module, Running#{Monitor => Id});
        {'DOWN', Monitor, process, _, Exit} ->
            {Id, Rest} = maps:take(Monitor, Running),
            ok = gen_tcp:send(Socket, [<<Id:32>> | verdict(Exit)]),
            serve(Socket, Module, Rest);
        {tcp_closed, Socket} ->
            halt()
    end.

verdict({verdict, ok}) -> [0];
verdict({verdict, {failed, Report}}) -> [1, Report];
verdict(Exit) -> [1, format("the process of the test exited with ~tp", [Exit])].

%% A test NIF returns its verdict, or asks for the next message of its process
%% and continues with it in a second call.
run(Module, Test, Step) ->
    case Module:Test(Step) of
        {next_message, Continuation} ->
            receive
                Message -> run(Module, Test, {Continuation, Message})
            after 5000 -> {failed, <<"no message arrived within 5 seconds">>}
            end;
        Verdict ->
            Verdict
    end.

format(Format, Arguments) ->
    unicode:characters_to_binary(io_lib:format(Format, Arguments)).
