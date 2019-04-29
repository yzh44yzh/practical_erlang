-module(st_game_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

-record(state, {
    transport,
    socket,
    player_srv
}).

start_link(Ref, _Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    lager:info("new connection, create player"),
    {ok, PlayerSrv} = supervisor:start_child(st_player_sup, [Socket]),
    Res = gen_tcp:controlling_process(Socket, PlayerSrv),
    lager:info("controlling_process res:~p", [Res]),
    State = #state{
        transport = Transport,
        socket = Socket,
        player_srv = PlayerSrv
    },
    loop(State).

loop(#state{socket = Socket, transport = Transport, player_srv = PlayerSrv} = State) ->
    {ok, ClientDisconnectTimeoutMin} = application:get_env(strategy, client_disconnect_timeout),
    ClientDisconnectTimeoutMilliSec = ClientDisconnectTimeoutMin * 60 * 1000,
    case Transport:recv(Socket, 0, ClientDisconnectTimeoutMilliSec) of
        {ok, <<"PING", _/binary>>} ->
            Reply = handle_ping(),
            Transport:send(Socket, Reply),
            loop(State);
        {ok, <<"AUTH ", LoginPass/binary>>} ->
            case handle_auth(PlayerSrv, LoginPass) of
                {ok, Reply} ->
                    Transport:send(Socket, Reply),
                    loop(State);
                {error, Reply} ->
                    Transport:send(Socket, Reply),
                    st_player_srv:stop(PlayerSrv),
                    ok = Transport:close(Socket)
            end;
        {ok, <<"GET PLAYERS", _/binary>>} ->
            Reply = handle_get_players(),
            Transport:send(Socket, Reply),
            loop(State);
        {ok, <<"GAME", _/binary>>} ->
            Reply = handle_game(),
            Transport:send(Socket, Reply),
            loop(State);
        {ok, UnknownData} ->
            Reply = <<"INVALID QUERY\n">>,
            lager:warning("Invalid Query:~p", [UnknownData]),
            Transport:send(Socket, Reply),
            loop(State);
        {error, Error} ->
            lager:info("close connection, ~p stop player", [Error]),
            st_player_srv:stop(PlayerSrv),
            ok = Transport:close(Socket)
    end.

handle_ping() -> <<"PONG\n">>.

handle_auth(PlayerSrv, LoginPass) ->
    [Login, Pass | _] = binary:split(LoginPass, [<<" ">>, <<"\n">>, <<"\r">>], [global]),
    lager:info("try to login with Login:~p Pass:~p", [Login, Pass]),
    Res = st_player_srv:auth(PlayerSrv, Login, Pass),
    case Res of
        ok -> {ok, <<"SUCCESS\n">>};
        error -> {error, <<"AUTH FAILED\n">>}
    end.


handle_get_players() ->
%%    AllSessions = st_player_storage:get_all_session(),
%%    Names = lists:filtermap(
%%        fun(PlayerSrv) ->
%%            case st_player_srv:get_name(PlayerSrv) of
%%                {ok, Name} -> {true, Name};
%%                {error, not_auth} -> false
%%            end,
%%            AllSessions
%%    ),
%%    NamesIO = list_concat_to_binary
    <<"[Vasja, Petja]\n">>.

handle_game() ->
    <<
        "+---+---+---+\n",
        "| A |   |   |\n",
        "+---+---+---+\n",
        "|   |   |   |\n",
        "+---+---+---+\n",
        "|   |   | B |\n",
        "+---+---+---+\n"
    >>.