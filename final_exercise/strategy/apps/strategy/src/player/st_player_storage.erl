-module(st_player_storage).
-behavior(gen_server).

-export([start_link/0, add_player/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

add_player(Socket, PlayerSrv) ->
    gen_server:call(?MODULE, {add_player, Socket, PlayerSrv}).

%%% gen_server API

-record(state, {
}).

init(no_args) ->
    lager:info("~p init", [?MODULE]),
    ets:new(?MODULE, [named_table]),
    State = #state{},
    {ok, State}.

handle_call({add_player, Socket, PlayerSrv}, _From, State) ->
    erlang:monitor(process, PlayerSrv),
    ets:insert(?MODULE, {PlayerSrv, Socket}),
    {reply, ok, State};

handle_call(_Request, _From, #state{} = State) ->
    {reply, ok, State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Info}, State) ->
    lager:info("Player down ~p ~p", [Pid, Info]),
    ets:delete(?MODULE, Pid),
    {noreply, State};

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
