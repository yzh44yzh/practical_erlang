-module(gs4).

-export([start/0, add/2, remove/2, check/2, show/1, stop/1]).
-export([loop/1]).

start() ->
    io:format("start ~p~n", [self()]),
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).

add(Pid, Item) ->
    Pid ! {add, Item}.

remove(Pid, Item) ->
    Pid ! {remove, Item}.

check(Pid, Item) ->
    Pid ! {check, Item}.

show(Pid) ->
    Pid ! show.

stop(Pid) ->
    Pid ! stop.

loop(State) ->
    io:format("~p enters loop ~n", [self()]),
    receive
        {add, Item} -> NewState = [Item | State],
                       ?MODULE:loop(NewState);
        {remove, Item} -> NewState = lists:delete(Item, State),
                          ?MODULE:loop(NewState);
        {check, Item} -> Res = lists:member(Item, State),
                         io:format("~p~n", [Res]),
                         ?MODULE:loop(State);
        show -> io:format("~p~n", [State]),
                      ?MODULE:loop(State);
        stop -> io:format("~p stops now ~n", [self()]);
        Msg -> io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
               ?MODULE:loop(State)
    end.
