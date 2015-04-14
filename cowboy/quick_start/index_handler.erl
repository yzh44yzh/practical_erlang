-module(index_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).


init(_Transport, Req, []) ->
    {ok, Req, undefined}.


handle(Req0, State) ->
    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    Body = <<"Hello from cowboy">>,
    {ok, Req1} = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req1, State}.


terminate(_Reason, _Req, _State) ->
    ok.
