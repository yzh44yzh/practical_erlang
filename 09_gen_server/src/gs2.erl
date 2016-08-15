-module(gs2).

-export([start/0, loop/1]).

start() ->
    io:format("start ~p~n", [self()]),
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).

loop(State) ->
    io:format("~p enters loop ~n", [self()]),
    receive
        {add, Item} -> NewState = [Item | State],
                       loop(NewState);
        {remove, Item} -> NewState = lists:delete(Item, State),
                          loop(NewState);
        {check, Item} -> Res = lists:member(Item, State),
                         io:format("~p~n", [Res]),
                         loop(State);
        show -> io:format("~p~n", [State]),
                      loop(State);
        stop -> io:format("~p stops now ~n", [self()]);
        Msg -> io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
               loop(State)
    end.
