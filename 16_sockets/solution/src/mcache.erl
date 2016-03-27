-module(mcache).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%% gen_server API

init([]) ->
    io:format("mcache started ~p~n", [self()]),
    {ok, no_state}.


handle_call({some, _Data}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call(Any, _From, State) ->
    lager:error("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast(Any, State) ->
    lager:error("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(Request, State) ->
    lager:error("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions
