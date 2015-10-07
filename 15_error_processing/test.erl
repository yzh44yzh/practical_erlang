-module(test).

-export([run/0]).

run() ->
    try
        1 + some_fun()
    catch
        throw:my_exception ->
            StackTrace = erlang:get_stacktrace(),
            io:format("~p", [StackTrace])
    end.


some_fun() ->
    2 + other_fun().


other_fun() ->
    throw(my_exception).
