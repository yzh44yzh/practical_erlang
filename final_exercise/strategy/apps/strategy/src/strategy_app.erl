-module(strategy_app).
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    start_ranch(),
    strategy_sup:start_link().


stop(_State) ->
    ok.


start_ranch() ->
    {ok, Port} = application:get_env(strategy, port),
    {ok, _} = ranch:start_listener(
        strategy_game_tcp_endpoint,
        ranch_tcp,
        [{port, Port}],
        st_game_protocol,
        []
    ).