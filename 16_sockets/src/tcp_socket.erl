-module(tcp_socket).

-export([start/1, listen/1, accept/1]).

start(Port) ->
    spawn(?MODULE, listen, [Port]).

listen(Port) ->
    io:format("server listens port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    spawn(?MODULE, accept, [ListenSocket]),
    timer:sleep(infinity),
    ok.

accept(ListenSocket) ->
    io:format("~p waiting for new client~n", [self()]),
    {ok, _Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, accept, [ListenSocket]),
    handle().

handle() ->
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            io:format("~p close client connection~n", [self()]),
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            io:format("~p handle ~p~n", [self(), Msg]),
            gen_tcp:send(Socket, Msg),
            handle()
    end.
