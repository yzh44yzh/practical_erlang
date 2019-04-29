-module(st_player_srv).
-behavior(gen_server).

-export([start_link/1, auth/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("st_player.hrl").

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

auth(PlayerSrv, Login, Pass) ->
    gen_server:call(PlayerSrv, {auth, Login, Pass}).

stop(PlayerSrv) ->
    gen_server:call(PlayerSrv, stop).


%%% gen_server API

-record(state, {
    socket :: pid(),
    player :: #player{}
}).

init(Socket) ->
    State = #state{
        socket = Socket,
        player = #player{}
    },
    st_player_storage:add_player(Socket, self()),
    lager:info("player created ~p ~p", [self(), State]),
    {ok, State}.

handle_call({auth, Login, <<"123">> = Pass}, _From, State = #state{player = Player}) ->
    %% go to database
    _FromDatabase = #{
        id => 42,
        login => Login,
        pass => Pass,
        rating => 4242
    },
    Player2 = Player#player{id = 42, name = Login, password = Pass, rating = 4242},
    State2 = State#state{player = Player2},
    {reply, ok, State2};

handle_call({auth, _Login, _Pass}, _From, State) ->
    {reply, error, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, #player{} = State) ->
    {reply, ok, State}.

handle_cast(_Request, #player{} = State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
