-module(not_found_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, []) ->
    {ok, Req, no_state}.


handle(Req, State) ->
    Body = <<"Not Found">>,
    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    {ok, Req1} = cowboy_req:reply(404, Headers, Body, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.
