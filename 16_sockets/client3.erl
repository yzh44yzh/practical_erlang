-module(client3).

-export([start/0, start/2, send/2, stop/1, client/2]).

start() ->
    start("localhost", 1234).

start(Host, Port) ->
    spawn(?MODULE, client, [Host, Port]).

send(Pid, Msg) ->
    Pid ! {send, Msg},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.

client(Host, Port) ->
    io:format("Client ~p connects to ~p:~p~n", [self(), Host, Port]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, 2}]),
    loop(Socket).

loop(Socket) ->
    receive
        {send, Msg} ->
            io:format("Client ~p send ~p~n", [self(), Msg]),
            gen_tcp:send(Socket, Msg),
            loop(Socket);
        {tcp, Socket, Msg} ->
            io:format("Client ~p got message: ~p~n", [self(), Msg]),
            loop(Socket);
        stop ->
            io:format("Client ~p closes connection and stops~n", [self()]),
            gen_tcp:close(Socket)
    after 200 ->
            loop(Socket)
    end.
