-module(sample2).

-export([run/0, system_process/0, worker/0]).

run() ->
    spawn(fun system_process/0),
    ok.


system_process() ->
    io:format("~p system process started~n", [self()]),
    process_flag(trap_exit, true),
    spawn_link(fun worker/0),
    receive
        Msg -> io:format("~p system process got message ~p~n", [self(), Msg])
    after 2000 -> ok
    end,
    ok.


worker() ->
    io:format("~p worker started~n", [self()]),
    timer:sleep(500),
    exit(some_reason),
    ok.
