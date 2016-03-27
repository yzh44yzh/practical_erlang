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
        {ok, Msg0} -> io:format("Socket #~p got message: ~p~n", [Id, Msg0]),
                      Msg = binary:part(Msg0, 0, byte_size(Msg0) - 2),
                      Reply = handle(parse_protocol(Msg)),
                      gen_tcp:send(Socket, <<Reply/binary, "\r\n">>),
                      handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.


parse_protocol(Data) ->
    Parts = binary:split(Data, <<" ">>),
    case Parts of
        [<<"SET">>, Rest] -> key_value(set, Rest);
        [<<"GET">>, Key] -> {get, Key};
        [<<"GETS">>, Keys] -> {gets, binary:split(Keys, <<" ">>, [global])};
        [<<"ADD">>, Rest] -> key_value(add, Rest);
        [<<"REPLACE">>, Rest] -> key_value(replace, Rest);
        [<<"APPEND">>, Rest] -> key_value(append, Rest);
        [<<"PREPEND">>, Rest] -> key_value(prepend, Rest);
        [<<"DELETE">>, Key] -> {delete, Key};
        _ -> unknown
    end.


key_value(Action, Data) ->
    case binary:split(Data, <<" ">>) of
        [Key, Value] -> {Action, Key, Value};
        _ -> unknown
    end.


handle({set, Key, Value}) ->
    mcache:set(Key, Value),
    <<"STORED">>;

handle({get, Key}) ->
    case mcache:get(Key) of
        {ok, Value} -> <<"VALUE ", Key/binary, " ", Value/binary, "\r\nEND">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle({gets, Keys}) ->
    {[], Res} = lists:foldl(
                  fun(R, {[Key | Rest], Acc}) ->
                          Value = case R of
                                      {ok, V} -> V;
                                      {error, not_found} -> <<"NOT FOUND">>
                                  end,
                          {Rest, <<Acc/binary, "VALUE ", Key/binary, " ", Value/binary, "\r\n">>}
                  end,
                  {Keys, <<>>},
                  mcache:gets(Keys)),
    <<Res/binary, "END">>;

handle({add, Key, Value}) ->
    case mcache:add(Key, Value) of
        ok -> <<"STORED">>;
        {error, exists} -> <<"EXISTS">>
    end;

handle({replace, Key, Value}) ->
    case mcache:replace(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle({append, Key, Value}) ->
    case mcache:append(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle({prepend, Key, Value}) ->
    case mcache:prepend(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle({delete, Key}) ->
    case mcache:delete(Key) of
        ok -> <<"DELETED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;

handle(unknown) ->
    <<"UKNOWN REQUEST">>.
