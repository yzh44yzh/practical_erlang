-module(gs3).

-export([start/0, loop/1]).

start() ->
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).

loop(State) ->
    receive
        {add, Item} -> io:format("~p adds ~p to its state~n", [self(), Item]),
                       NewState = [Item | State],
                       ?MODULE:loop(NewState);
        {remove, Item} -> NewState = case lists:member(Item, State) of
                                         true -> lists:delete(Item, State);
                                         false -> io:format("I have no ~p~n", [Item]),
                                                  State
                                     end,
                          ?MODULE:loop(NewState);
        show_items -> io:format("my items is ~p~n", [State]),
                      ?MODULE:loop(State);
        stop -> io:format("~p stops now ~n", [self()]);
        _Any -> ?MODULE:loop(State)
    end.
