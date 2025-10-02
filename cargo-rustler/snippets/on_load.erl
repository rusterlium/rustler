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
                    filename:flatten(filename:join([AbsDir, <<"..">>, <<"priv">>]))
            end
    end,

    Attributes = ?MODULE:module_info(attributes),

    Filename =
        case proplists:get_value(nif_lib_name, Attributes) of
            undefined ->
                error({missing_attribute, nif_lib_name});
            NifLibName ->
                filename:join([PrivDir, <<"native">>, NifLibName])
        end,

    case proplists:get_value(nif_load_hook, Attributes) of
        undefined ->
            ok;
        {Mod, FuncName} ->
            case erlang:function_exported(Mod, FuncName, 2) of
                true ->
                    Mod:FuncName(?MODULE, Filename);
                false ->
                    error({nif_hook_not_defined, Mod, FuncName})
            end
    end,

    LoadInfo = proplists:get_value(nif_load_info, Attributes, 0),
    erlang:load_nif(Filename, LoadInfo).
