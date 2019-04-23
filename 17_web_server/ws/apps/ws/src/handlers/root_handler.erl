-module(root_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Headers = #{
        <<"content-type">> => <<"text/html">>
    },
    Body = <<
        "<html><head>"
        "<link href='/static/main.css' rel='stylesheet' type='text/css'>"
        "</head><body>"
        "<h1>Hello from Cowboy</h1>"
        "<p>This is root_handler</p>"
        "</body></html>"
    >>,
    Req1 = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req1, State}.