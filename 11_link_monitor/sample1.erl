-module(sample1).

-export([run/0, run_and_crash/0, work/1, work_and_crash_one/1]).

run() ->
    [spawn_link(?MODULE, work, [Id]) || Id <- lists:seq(0, 5)],
    ok.

work(Id) ->
    io:format("~p ~p started~n", [Id, self()]),
    timer:sleep(1000),
    io:format("~p ~p stopped~n", [Id, self()]),
    ok.


run_and_crash() ->
    [spawn_link(?MODULE, work_and_crash_one, [Id]) || Id <- lists:seq(0, 5)],
    ok.

work_and_crash_one(Id) ->
    io:format("~p ~p started~n", [Id, self()]),
    if
        Id == 3 ->
            io:format("~p ~p exiting~n", [Id, self()]),
            exit(for_some_reason);
        true -> ok
    end,
    timer:sleep(1000),
    io:format("~p ~p stopped~n", [Id, self()]),
    ok.
