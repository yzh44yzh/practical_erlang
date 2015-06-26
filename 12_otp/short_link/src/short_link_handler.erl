-module(short_link_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, []) ->
    {ok, Req, no_state}.


handle(Req, State) ->
    {Path, _} = cowboy_req:path(Req),
    {GetParams, _} = cowboy_req:qs_vals(Req),
    {ok, PostParams, _} = cowboy_req:body_qs(Req),
    AllParams = GetParams ++ PostParams,
    lager:info("all params:~p", [AllParams]),

    {Code, Body} = case Path of
        <<"/create">> -> create_short(AllParams);
        <<"/create/">> -> create_short(AllParams);
        <<"/get">> -> get_long(AllParams);
        <<"/get/">> -> get_long(AllParams)
    end,

    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    {ok, Req1} = cowboy_req:reply(Code, Headers, Body, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

create_short(Params) ->
    case proplists:get_value(<<"link">>, Params) of
        undefined -> {409, <<"invalid request">>};
        Link -> Body = short_link:create_short(Link),
                {201, Body}
    end.

get_long(Params) ->
    case proplists:get_value(<<"link">>, Params) of
        undefined -> {409, <<"invalid request">>};
        Link -> case short_link:get_long(Link) of
                    {ok, L} -> {200, L};
                    {error, not_found} -> {404, <<"Not Found">>}
                end
    end.
