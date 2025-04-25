-on_load(on_load/0).

on_load() ->
    PrivDir = case application:get_application(?MODULE) of
        {ok, App} ->
            code:priv_dir(App);
        undefined ->
            case code:which(?MODULE) of
                non_existing ->
                    error(nif_module_not_found);
                BeamPath ->
                    AbsDir = filename:dirname(filename:absname(BeamPath)), 
                    filename:flatten(filename:join([AbsDir, "..", "priv"]))
            end
    end,

    Filename = filename:join([PrivDir, "native", ?NIF_NAME]),
    erlang:load_nif(Filename, ?NIF_LOAD_INFO).

