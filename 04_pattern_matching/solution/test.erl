-module(test).

-export([run/0]).

run() ->
    case tic_tac_toe_test:test() of
        ok -> init:stop(0);
        error -> init:stop(1)
    end.
