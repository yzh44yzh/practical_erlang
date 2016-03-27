-module(mcache).
-behavior(gen_server).

-export([start_link/0, set/2, get/1, gets/1, add/2, replace/2, append/2, prepend/2, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
          table :: ets:tid()
         }).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec set(binary(), binary()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).


-spec get(binary()) -> {ok, binary()} | {error, not_found}.
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] -> {error, not_found};
        [{Key, Val}] -> {ok, Val}
    end.


-spec gets([binary()]) -> [{ok, binary()} | {error, not_found}].
gets(Keys) ->
    lists:map(fun get/1, Keys).



-spec add(binary(), binary()) -> ok | {error, exists}.
add(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, _} -> {error, exists};
        {error, not_found} -> set(Key, Value)
    end.


-spec replace(binary(), binary()) -> ok | {error, not_found}.
replace(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, _} -> set(Key, Value);
        {error, not_found} -> {error, not_found}
    end.


-spec append(binary(), binary()) -> ok | {error, not_found}.
append(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, OldValue} -> set(Key, <<OldValue/binary, Value/binary>>);
        {error, not_found} -> {error, not_found}
    end.


-spec prepend(binary(), binary()) -> ok | {error, not_found}.
prepend(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, OldValue} -> set(Key, <<Value/binary, OldValue/binary>>);
        {error, not_found} -> {error, not_found}
    end.


-spec delete(binary()) -> ok | {error, not_found}.
delete(Key) ->
    case ?MODULE:get(Key) of
        {ok, _} -> gen_server:call(?MODULE, {delete, Key});
        {error, not_found} -> {error, not_found}
    end.


%%% gen_server API

init([]) ->
    io:format("mcache started ~p~n", [self()]),
    T = ets:new(?MODULE, [set, protected, named_table]),
    {ok, #state{table = T}}.


handle_call({set, Key, Value}, _From, #state{table = T} = State) ->
    ets:insert(T, {Key, Value}),
    {reply, ok, State};

handle_call({delete, Key}, _From, #state{table = T} = State) ->
    ets:delete(T, Key),
    {reply, ok, State};

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
