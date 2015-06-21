-module(gs6).

-export([start/0, add_item/2, remove_item/2, show_items/1, stop/1, loop/1]).


start() ->
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).


add_item(Pid, Item) ->
    call(Pid, {add, Item}).


remove_item(Pid, Item) ->
    call(Pid, {remove, Item}).


show_items(Pid) ->
    call(Pid, show_items).


call(Pid, Msg) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {Msg, self(), MRef},
    receive
        {reply, MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            {error, Reason}
    after 5000 ->
            erlang:demonitor(MRef, [flush]),
            no_reply
    end.


stop(Pid) ->
    Pid ! stop,
    ok.


loop(State) ->
    receive
        {{add, Item}, From, Ref} ->
            NewState = [Item | State],
            From ! {reply, Ref, ok},
            ?MODULE:loop(NewState);
        {{remove, Item}, From, Ref} ->
            {Reply, NewState} = case lists:member(Item, State) of
                                    true -> {ok, lists:delete(Item, State)};
                                    false -> {{error, not_exist}, State}
                                end,
            %% Reply = 55 / some, % want to crash here
            From ! {reply, Ref, Reply},
            ?MODULE:loop(NewState);
        {show_items, From, Ref} ->
            From ! {reply, Ref, State},
            ?MODULE:loop(State);
        stop -> ok;
        _Any -> ?MODULE:loop(State)
    end.
