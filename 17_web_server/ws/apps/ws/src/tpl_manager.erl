-module(tpl_manager).

-export([get_template/1]).

get_template(Name) ->
    case cache:get(tpl_cache, Name) of
        undefined ->
            io:format("~p not found in cache~n", [Name]),
            PrivDir = code:priv_dir(ws),
            {ok, Tpl} = file:read_file(PrivDir ++ "/tpl/" ++ Name ++ ".html"),
            cache:put(tpl_cache, Name, Tpl),
            Tpl;
        Tpl ->
            io:format("~p found in cache~n", [Name]),
            Tpl
    end.


