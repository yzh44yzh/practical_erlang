-module(test).

-export([run/0]).

run() ->
    case chat_room_manager_test:test() of
        ok -> init:stop(0);
        error -> init:stop(1)
    end.
