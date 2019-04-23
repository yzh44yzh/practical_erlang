# Web Server

create fresh project with rebar
```
rebar3 new release ws
```


## cowboy

Docs:
https://ninenines.eu/docs/

Package:
https://hex.pm/packages/cowboy

add deps to rebar.config
```
{deps, [
    {cowboy, "2.6.3"}
]}.
```

add ranch to ws.app.src
```
    {applications, [
        kernel,
        stdlib,
        ranch
    ]},
```


## simple routing and simple handlers

ws_app.erl
```
start(_StartType, _StartArgs) ->
    init_cowboy(),
    ws_sup:start_link().

...

init_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", root_handler, []},
            {"/ping", ping_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.
```

handlers/root_handler.erl
```
-module(root_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Headers = #{
        <<"content-type">> => <<"text/html">>
    },
    Body = <<
        "<h1>Hello from Cowboy</h1>"
        "<p>This is root_handler</p>"
    >>,
    Req1 = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req1, State}.
```

same code for ping_handler

Run:
```
rebar3 compile
rebar3 shell
```

Check:
http://localhost:8080/
http://localhost:8080/ping


## fix configuration

add port to sys.config
also set error level for sasl in sys.config

config/sys.config
```
[
    {ws, [
        {port, 8080}
    ]},

    {sasl, [
        {errlog_type, error}
    ]}
].
```

ws_app.erl
```
init_cowboy() ->
    {ok, Port} = application:get_env(ws, port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", root_handler, []},
            {"/ping", ping_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.
```

## statis files

priv/www/main.css
```
body {
    color: #f00;
}
```

src/handlers/root_handler.erl
```
    Body = <<
        "<html><head>"
        "<link href='/static/main.css' rel='stylesheet' type='text/css'>"
        "</head><body>"
        "<h1>Hello from Cowboy</h1>"
        "<p>This is root_handler</p>"
        "</body></html>"
    >>,
```

src/ws_app.erl
```
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, ws, "www"}},
            {"/", root_handler, []},
            {"/ping", ping_handler, []}
        ]}
    ]),
```


## routing and handler details

https://ninenines.eu/docs/en/cowboy/2.6/guide/routing/


## request obj

## templating
mustache
https://hex.pm/packages/bbmustache

## templates cache
cache
https://hex.pm/packages/cache

## json api
json request, json responce
jsx
https://hex.pm/packages/jsx

## validation pipeline
erlz
https://github.com/yzh44yzh/erlz
