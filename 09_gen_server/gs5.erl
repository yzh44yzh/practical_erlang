-module(gs5).

-export([start/0, add_item/2, remove_item/2, show_items/1, stop/1, loop/1]).


start() ->
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).


add_item(Pid, Item) ->
    Ref = make_ref(),
    Pid ! {add, self(), Ref, Item},
    receive
        {reply, Ref, Reply} -> Reply
    end.


remove_item(Pid, Item) ->
    Ref = make_ref(),
    Pid ! {remove, self(), Ref, Item},
    receive
        {reply, Ref, Reply} -> Reply
    end.


show_items(Pid) ->
    Ref = make_ref(),
    Pid ! {show_items, self(), Ref},
    receive
        {reply, Ref, Reply} -> Reply
    end.


stop(Pid) ->
    Pid ! stop,
    ok.


loop(State) ->
    receive
        {add, From, Ref, Item} ->
            NewState = [Item | State],
            From ! {reply, Ref, ok},
            ?MODULE:loop(NewState);
        {remove, From, Ref, Item} ->
            {Reply, NewState} = case lists:member(Item, State) of
                                    true -> {ok, lists:delete(Item, State)};
                                    false -> {{error, not_exist}, State}
                                end,
            From ! {reply, Ref, Reply},
            ?MODULE:loop(NewState);
        {show_items, From, Ref} ->
            From ! {reply, Ref, State},
            ?MODULE:loop(State);
        stop -> ok;
        _Any -> ?MODULE:loop(State)
    end.
