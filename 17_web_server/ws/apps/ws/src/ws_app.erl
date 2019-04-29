-module(ws_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    init_cowboy(),
    {ok, _} = cache:start_link(tpl_cache, [{n, 10}, {ttl, 60}]),
    ws_sup:start_link().


stop(_State) ->
    ok.


init_cowboy() ->
    {ok, Port} = application:get_env(ws, port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/static/[...]", cowboy_static, {priv_dir, ws, "www"}},
            {"/", root_handler, []},
            {"/user/:user_id", user_handler, []},
            {"/ping", ping_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.