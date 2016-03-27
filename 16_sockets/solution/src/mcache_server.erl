-module(mcache_server).

-export([start_link/0, accept/2]).

start_link() ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, PoolSize} = application:get_env(mcache, accept_pool_size),
    io:format("start mcache_server at port ~p with pool ~p~n", [Port, PoolSize]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, PoolSize)],
    timer:sleep(infinity),
    ok.

accept(Id, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket #~p, session started~n", [Id]),
    handle_connection(Id, ListenSocket, Socket).

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Msg} -> io:format("Socket #~p got message: ~p~n", [Id, Msg]),
                     gen_tcp:send(Socket, Msg),
                     handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.
