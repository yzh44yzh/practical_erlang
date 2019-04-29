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


## routing and simple handlers

https://ninenines.eu/docs/en/cowboy/2.6/guide/routing/

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


## request obj

https://ninenines.eu/docs/en/cowboy/2.6/guide/req/

Выведем на консоль:
```
init(Req0, State) ->
    io:format("~p~n", [Req0]),
```

Добавим еще один url на root_handler
```
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, ws, "www"}},
            {"/", root_handler, []},
            {"/user/:user_id", root_handler, []},
            {"/ping", ping_handler, []}
        ]}
    ]),
```

Посмотрим запросы:
http://localhost:8080/
http://localhost:8080/user/42
http://localhost:8080/user/42?a=b&c=d

cowboy_req модуль

```
    UserId = cowboy_req:binding(user_id, Req0),
    io:format("UserId:~p~n", [UserId]),
    GetParams = cowboy_req:parse_qs(Req0),
    io:format("GetParams:~p~n", [GetParams]),
```

try cowboy_req:match_qs

Body
https://ninenines.eu/docs/en/cowboy/2.6/guide/req_body/
```
    case cowboy_req:has_body(Req0) of
        true ->
            Length = cowboy_req:body_length(Req0),
            io:format("Body Length:~p~n", [Length]),
            {ok, ReqBody, _Req} = cowboy_req:read_body(Req0),
            io:format("Body:~p~n", [ReqBody]),
            ok;
        false -> io:format("no body~n")
    end,
```

Query with curl
```
curl "http://localhost:8080/user/42" -d '{"user_id": 42, "user_name": "Bob"}'
```


## templating

mustache
https://hex.pm/packages/bbmustache

add deps to rebar.config
```
{deps, [
    {cowboy, "2.6.3"},
    {bbmustache, "1.7.0"}
]}.
```

add user_handler
```
-module(user_handler).

-export([init/2]).

init(Req0, State) ->
    Bindings = cowboy_req:bindings(Req0),
    UserId = maps:get(user_id, Bindings, <<"0">>),
    Info = get_user_info(UserId),

    PrivDir = code:priv_dir(ws),
    {ok, Tpl} = file:read_file(PrivDir ++ "/tpl/user.html"),
    Body = bbmustache:render(Tpl, Info),

    Headers = #{
        <<"content-type">> => <<"text/html">>
    },

    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, State}.


get_user_info(UserId) ->
    #{
        "user_id" => UserId,
        "username" => <<"Bob">>,
        "attributes" => [
            #{"attr_name" => "height", "attr_value" => 182},
            #{"attr_name" => "weight", "attr_value" => 70}
        ]
    }.
```

change routing to user handler
```
{"/static/[...]", cowboy_static, {priv_dir, ws, "www"}},
{"/", root_handler, []},
{"/user/:user_id", user_handler, []},
{"/ping", ping_handler, []}
```

create template
priv/tpl/user.html
```
<html>
<head>
    <title>Hello {{username}}!</title>
</head>
<body>
<h1>Hello {{username}}</h1>
<p>ID: {{user_id}}</p>

<ul>
{{#attributes}}
<li>{{attr_name}} {{attr_value}}</li>
{{/attributes}}
</ul>

</body>
</html>
```

user handler get user info:
```
    Bindings = cowboy_req:bindings(Req0),
    UserId = maps:get(user_id, Bindings, <<"0">>),
    Info = get_user_info(UserId),
```
reads template file:
```
    PrivDir = code:priv_dir(ws),
    {ok, Tpl} = file:read_file(PrivDir ++ "/tpl/user.html"),
```
and parses template:
```
    Body = bbmustache:render(Tpl, Info),
```

## templates cache

cache
https://hex.pm/packages/cache

add deps to rebar.config
```
{deps, [
    {cowboy, "2.6.3"},
    {bbmustache, "1.7.0"},
    {cache, "2.3.1"}
]}.
```

cache нужно добавить в зависимые приложения в ws.app.src, чтобы он запустился при старте ноды:
```
{application, ws, [
    {description, "An OTP application"},
    {vsn, "0.1.0"},
    {registered, []},
    {mod, {ws_app, []}},
    {applications, [
        kernel,
        stdlib,
        ranch,
        cache
    ]},
    {env,[]},
    {modules, []}
 ]}.
```

И нужно создать экземпляр кэша, в котором мы будем хранить шаблоны:
```
start(_StartType, _StartArgs) ->
    init_cowboy(),
    {ok, _} = cache:start_link(tpl_cache, [{n, 10}, {ttl, 60}]),
    ws_sup:start_link().
```
TODO: его надо запустить под супервизором, а не так.

create module tpl_manager:
```
-module(tpl_manager).

-export([get_template/1]).

get_template(Name) ->
    case cache:get(tpl_cache, Name) of
        undefined ->
            io:format("~p not found in cache~n", [Name]),
            PrivDir = code:priv_dir(ws),
            {ok, Tpl} = file:read_file(PrivDir ++ "/tpl/" ++ Name ++ ".html"),
            cache:put(tpl_cache, Name, Tpl),
            Tpl;
        Tpl ->
            io:format("~p found in cache~n", [Name]),
            Tpl
    end.
```

Ищем шаблон в кеше. Если не находим, читаем шаблон из файла и сохраняем в кэш.
Отдаем шаблон.

TODO:
Здесь не обрабатывается ситуация, когда указано неправильное имя шаблона.
Такое надо обрабатывать.

user_hander берет шаблон через tpl_manager, а не напрямую из файла:
```
    Tpl = tpl_manager:get_template("user"),
    Body = bbmustache:render(Tpl, Info),
```


## json api

jsx
https://hex.pm/packages/jsx


## validation pipeline

erlz
https://github.com/yzh44yzh/erlz
