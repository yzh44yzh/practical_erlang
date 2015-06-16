-module(test).

-export([run/0]).

run() ->
    case main:test() of
        ok -> init:stop(0);
        error -> init:stop(1)
    end.
