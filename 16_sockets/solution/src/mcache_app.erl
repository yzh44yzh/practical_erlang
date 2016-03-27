-module(mcache_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).


-spec(start() -> ok).
start() ->
    application:start(mcache),
    ok.


-spec(start(term(), term()) -> {ok, pid()}).
start(_StartType, _StartArgs) ->
    io:format("start mcache_app~n"),
    mcache_sup:start_link().


-spec(stop(term()) -> ok).
stop(_State) ->
    ok.
