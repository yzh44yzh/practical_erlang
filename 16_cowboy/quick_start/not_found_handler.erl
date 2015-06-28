-module(not_found_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).


init(_Transport, Req, []) ->
    {ok, Req, undefined}.


handle(Req0, State) ->
    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    Body = "Not Found",
    {ok, Req1} = cowboy_req:reply(404, Headers, Body, Req0),
    {ok, Req1, State}.


terminate(_Reason, _Req, _State) ->
    ok.
