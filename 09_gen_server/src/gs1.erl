-module(gs1).

-export([start/0]).

start() ->
    io:format("start ~p~n", [self()]),
    spawn(fun loop/0).

loop() ->
    io:format("~p enters loop ~n", [self()]),
    receive
        Msg -> io:format("~p receive ~p~n", [self(), Msg]),
               loop()
    end.
