-module(ping_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Headers = #{
        <<"content-type">> => <<"text/html">>
    },
    Body = <<
        "<h1>Hello from Cowboy</h1>"
        "<p>This is ping_handler</p>"
    >>,
    Req1 = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req1, State}.