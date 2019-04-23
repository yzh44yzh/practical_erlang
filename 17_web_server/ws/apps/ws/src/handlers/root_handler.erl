-module(root_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% io:format("~p~n", [Req0]),
    UserId = cowboy_req:binding(user_id, Req0),
    io:format("UserId:~p~n", [UserId]),
    GetParams = cowboy_req:parse_qs(Req0),
    io:format("GetParams:~p~n", [GetParams]),

    case cowboy_req:has_body(Req0) of
        true ->
            Length = cowboy_req:body_length(Req0),
            io:format("Body Length:~p~n", [Length]),
            {ok, ReqBody, _Req} = cowboy_req:read_body(Req0),
            io:format("Body:~p~n", [ReqBody]),
            ok;
        false -> io:format("no body~n")
    end,

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