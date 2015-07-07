-module(short_link_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

-spec(start() -> ok).
start() ->
    lager:start(),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(short_link),
    ok.


-spec(start(term(), term()) -> {ok, pid()}).
start(_StartType, _StartArgs) ->
    lager:info("start short_link_app"),

    Routing = cowboy_router:compile(routing()),
    Res = cowboy:start_http(http, 100, [{port, 8080}],
                      [{env, [{dispatch, Routing}]}]),
    lager:info("cowboy: ~p", [Res]),
    short_link_sup:start_link().


-spec(stop(term()) -> ok).
stop(_State) ->
    ok.


routing() ->
    [{'_', [
            {"/create", short_link_handler, []},
            {"/get", short_link_handler, []},
            {"/ping", ping_handler, []},
            {'_', not_found_handler, []}
           ]}].
