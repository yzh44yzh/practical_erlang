-module(ws_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    init_cowboy(),
    ws_sup:start_link().


stop(_State) ->
    ok.


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