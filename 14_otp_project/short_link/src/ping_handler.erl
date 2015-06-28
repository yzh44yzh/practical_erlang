-module(ping_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, []) ->
    {ok, Req, no_state}.


handle(Req, State) ->
    Body = <<"Short Link Service. Version 777">>,
    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    {ok, Req1} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
