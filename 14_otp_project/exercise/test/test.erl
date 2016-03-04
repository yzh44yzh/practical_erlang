-module(test).

-export([run/0]).

run() ->
    case my_crypt_tests:test() of
        ok -> init:stop(0);
        error -> init:stop(1)
    end.
