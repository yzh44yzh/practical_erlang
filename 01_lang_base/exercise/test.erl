-module(test).

-export([run/0, run/1]).

run() ->
    Modules = [task_1, task_2, task_3, task_4, task_5],
    Res = lists:map(fun(M) -> eunit:test(M) end, Modules),
    case Res of
        [ok,ok,ok,ok,ok] -> init:stop(0);
        _ -> init:stop(1)
    end.


run([M, F]) ->
    io:format("run test ~p:~p~n", [M, F]),
    Res = erlang:apply(M, F, []),
    io:format("result: ~p~n~n", [Res]),
    case Res of
        ok -> init:stop(0);
        error -> init:stop(1)
    end.
