-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(WorkerId) ->
    gen_server:start_link(?MODULE, [WorkerId], []).

ping(Pid) ->
    gen_server:call(Pid, ping).


init([WorkerId]) ->
    io:format("worker ~p:~p init~n", [WorkerId, self()]),
    {ok, WorkerId}.


handle_call(ping, _From, State) ->
    WorkerId = State,
    Reply = {WorkerId, self()},
    {reply, Reply, State};

handle_call(_Any, _From, State) ->
    {noreply, State}.


handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(Reason, State) ->
    WorkerId = State,
    io:format("worker ~p:~p terminate ~p~n", [WorkerId, self(), Reason]),
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
