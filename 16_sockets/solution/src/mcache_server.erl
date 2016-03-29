-module(mcache_server).
-behavior(gen_server).

-export([start_link/0, accept/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, PoolSize} = application:get_env(mcache, accept_pool_size),
    io:format("start mcache_server at port ~p with pool ~p~n", [Port, PoolSize]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, PoolSize)],
    {ok, ListenSocket}.


handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


accept(Id, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [Id]),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Socket #~p session started~n", [Id]),
            handle_connection(Id, ListenSocket, Socket);
        E -> io:format("Socket #~p can't accept client ~p~n", [Id, E])
    end.

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Msg0} ->
            Msg = binary:part(Msg0, 0, byte_size(Msg0) - 2),
            Reply = handle(parse_protocol(Msg)),
            gen_tcp:send(Socket, <<Reply/binary, "\r\n">>),
            handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Socket #~p session closed ~n", [Id]),
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
    <<"UNKNOWN REQUEST">>.
