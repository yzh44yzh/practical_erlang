-module(gs9).

-export([start/0, add/2, remove/2, check/2, show/1, stop/1]).
-export([loop/1]).

start() ->
    io:format("start ~p~n", [self()]),
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).

add(Pid, Item) ->
    call(Pid, {add, Item}).

remove(Pid, Item) ->
    call(Pid, {remove, Item}).

check(Pid, Item) ->
    call(Pid, {check, Item}).

show(Pid) ->
    call(Pid, show).

stop(Pid) ->
    Pid ! stop.

call(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {call, Ref, self(), Msg},
    receive
        {reply, Ref, Reply} -> Reply
    after 5000 -> noreply
    end.

loop(State) ->
    io:format("~p enters loop ~n", [self()]),
    receive
        {call, Ref, From, Msg} -> {Reply, NewState} = handle_call(Msg, State),
                             From ! {reply, Ref, Reply},
                             ?MODULE:loop(NewState);
        stop -> io:format("~p stops now ~n", [self()]);
        Msg -> io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
               ?MODULE:loop(State)
    end.

handle_call({add, Item}, State) ->
    NewState = [Item | State],
    {ok, NewState};

handle_call({remove, Item}, State) ->
    NewState = lists:delete(Item, State),
    {ok, NewState};

handle_call({check, Item}, State) ->
    Res = lists:member(Item, State),
    {Res, State};

handle_call(show, State) ->
    {State, State}.
