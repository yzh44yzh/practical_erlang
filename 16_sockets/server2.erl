-module(server2).

-export([start/0, start/1, server/1, accept/2]).

start() ->
    start(1234).

start(Port) ->
    spawn(?MODULE, server, [Port]),
    ok.

server(Port) ->
    io:format("start server at port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    timer:sleep(infinity),
    ok.

accept(Id, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket #~p, session started~n", [Id]),
    handle_connection(Id, ListenSocket, Socket).

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 2) of
        {ok, Header} -> <<Size:16/integer>> = Header,
                        {ok, Msg} = gen_tcp:recv(Socket, Size),
                        io:format("Socket #~p got message: ~p~n", [Id, Msg]),
                        gen_tcp:send(Socket, Msg),
                        handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.
