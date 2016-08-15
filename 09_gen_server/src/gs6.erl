-module(gs6).

-export([start/0, add/2, remove/2, check/2, show/1, stop/1]).
-export([loop/1]).

start() ->
    io:format("start ~p~n", [self()]),
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).

add(Pid, Item) ->
    Pid ! {self(), {add, Item}},
    receive
        {reply, Reply} -> Reply
    after 5000 -> noreply
    end.

remove(Pid, Item) ->
    Pid ! {self(), {remove, Item}},
    receive
        {reply, Reply} -> Reply
    after 5000 -> noreply
    end.

check(Pid, Item) ->
    Pid ! {self(), {check, Item}},
    receive
        {reply, Reply} -> Reply
    after 5000 -> noreply
    end.

show(Pid) ->
    Pid ! {self(), show},
    receive
        {reply, Reply} -> Reply
    after 5000 -> noreply
    end.

stop(Pid) ->
    Pid ! stop.

loop(State) ->
    io:format("~p enters loop ~n", [self()]),
    receive
        {From, {add, Item}} -> NewState = [Item | State],
                               From ! {reply, ok},
                               ?MODULE:loop(NewState);
        {From, {remove, Item}} -> NewState = lists:delete(Item, State),
                                  From ! {reply, ok},
                                  ?MODULE:loop(NewState);
        {From, {check, Item}} -> Res = lists:member(Item, State),
                                 From ! {reply, Res},
                                 ?MODULE:loop(State);
        {From, show} -> From ! {reply, State},
                        ?MODULE:loop(State);
        stop -> io:format("~p stops now ~n", [self()]);
        Msg -> io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
               ?MODULE:loop(State)
    end.
