-module(sample_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).


init(_Transport, Req, _Opts) ->
    {ok, Req, undefined_state}.


handle(Req, State) ->
    {Host, _} = cowboy_req:host(Req),
    {Path, _} = cowboy_req:path(Req),
    {Url, _} = cowboy_req:url(Req),
    {QueryString, _} = cowboy_req:qs(Req),
    {GetParams, _} = cowboy_req:qs_vals(Req),
    {ok, PostParams, _} = cowboy_req:body_qs(Req),
    {UserId, _} = cowboy_req:binding(user_id, Req),
    {UserName, _} = cowboy_req:qs_val(<<"user_name">>, Req, <<"guest">>),

    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    Body = ["<h1>Sample http handler</h1>",
            "<p>Host: ", Host, "</p>",
            "<p>Path: ", Path, "</p>",
            "<p>Url: ", Url, "</p>",
            "<p>QueryString: ", QueryString, "</p>",
            "<p>Get Params: ", show_params(GetParams), "</p>",
            "<p>Post Params: ", show_params(PostParams), "</p>",
            "<p>User Id: ", UserId, "</p>",
            "<p>User Name: ", UserName, "</p>"
           ],

    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.


terminate(Reason, Req, State) ->
    ok.


show_params(List) ->
    P = lists:map(fun({Key, Value}) ->
                      ["<li><b>", Key, "</b>: ", Value, "</li>"]
              end, List),
    ["<ul>", P, "</ul>"].
