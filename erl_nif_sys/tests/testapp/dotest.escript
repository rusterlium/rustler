
%% dotest.script
%%
%% Compile the test module in-memory and execute it's test function.
%%

source() -> "mynifmod.erl".

main([]) ->
	io:format("Compiling test module ~s\n", [source()]),
	case compile:file(source(), [binary,verbose,report_errors,report_warnings,debug_info]) of
		{ok, Name, Bin} ->
			run_tests(Name, Bin);

		{ok, Name, Bin, Warnings} ->
			show([], Warnings),
			run_tests(Name, Bin);

		error ->
			halt(1);

		{error, Errors, Warnings} ->
			show(Errors, Warnings),
			halt(1)
	end.

run_tests(Name, Bin) ->
	io:format("Running tests in test module ~s\n", [source()]),
	{module, _} = code:load_binary(Name, source(), Bin),
	case Name:test() of
		ok    -> halt(0);
		error -> halt(1)
	end.

show(Errors, Warnings) ->
	io:format("errors = ~p\n", [Errors]),
	io:format("warnings = ~p\n", [Warnings]).
