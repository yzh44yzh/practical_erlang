-module(mcache_tests).

-include_lib("eunit/include/eunit.hrl").


mcache_test() ->
    application:ensure_all_started(mcache),
    Client_1 = get_connection(),
    Client_2 = get_connection(),
    Client_3 = get_connection(),
    Queries =
        [
         {1,  Client_1, <<"PING">>, <<"UNKNOWN REQUEST">>}
        ,{2,  Client_2, <<"SET my_num 42">>, <<"STORED">>}
        ,{3,  Client_3, <<"GET my_num">>, <<"VALUE my_num 42\r\nEND">>}
        ,{4,  Client_1, <<"GETS my_num my_str some_key">>,
          <<"VALUE my_num 42\r\nVALUE my_str NOT FOUND\r\nVALUE some_key NOT FOUND\r\nEND">>}
        ,{5,  Client_1, <<"GET my_str">>, <<"NOT FOUND">>}
        ,{6,  Client_2, <<"SET my_str Hello World!">>, <<"STORED">>}
        ,{7,  Client_2, <<"GET my_str">>, <<"VALUE my_str Hello World!\r\nEND">>}
        ,{8,  Client_1, <<"GETS my_str some_key">>,
          <<"VALUE my_str Hello World!\r\nVALUE some_key NOT FOUND\r\nEND">>}
        ,{9,  Client_3, <<"ADD my_num 77">>, <<"EXISTS">>}
        ,{10, Client_1, <<"ADD key1 77">>, <<"STORED">>}
        ,{11, Client_3, <<"ADD key1 77">>, <<"EXISTS">>}
        ,{12, Client_1, <<"REPLACE key2 88">>, <<"NOT FOUND">>}
        ,{13, Client_2, <<"ADD key2 88">>, <<"STORED">>}
        ,{14, Client_1, <<"GET key2">>, <<"VALUE key2 88\r\nEND">>}
        ,{15, Client_2, <<"REPLACE key2 99">>, <<"STORED">>}
        ,{16, Client_2, <<"GET key2">>, <<"VALUE key2 99\r\nEND">>}
        ,{17, Client_2, <<"GETS my_num key1 key2">>,
          <<"VALUE my_num 42\r\nVALUE key1 77\r\nVALUE key2 99\r\nEND">>}
        ,{18, Client_3, <<"APPEND key1 123">>, <<"STORED">>}
        ,{19, Client_3, <<"GET key1">>, <<"VALUE key1 77123\r\nEND">>}
        ,{20, Client_1, <<"APPEND some_key 321">>, <<"NOT FOUND">>}
        ,{21, Client_2, <<"PREPEND key2 321">>, <<"STORED">>}
        ,{22, Client_3, <<"GET key2">>, <<"VALUE key2 32199\r\nEND">>}
        ,{23, Client_1, <<"PREPEND some_key 789">>, <<"NOT FOUND">>}
        ,{24, Client_3, <<"GETS key42 some_key">>,
          <<"VALUE key42 NOT FOUND\r\nVALUE some_key NOT FOUND\r\nEND">>}
        ,{25, Client_2, <<"QUIT">>, <<"UNKNOWN REQUEST">>}
        ],
    lists:map(fun check_query/1, Queries),
    gen_tcp:close(Client_1),
    gen_tcp:close(Client_2),
    gen_tcp:close(Client_3),
    ok.


get_connection() ->
    Host = "localhost",
    Port = 1234,
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, line}]),
    Socket.


check_query({Num, Socket, Query, WaitReply}) ->
    io:format("Query ~p~n~p~n", [Num, Query]),
    gen_tcp:send(Socket, <<Query/binary, "\r\n">>),
    Lines = binary:split(WaitReply, <<"\r\n">>, [global]),
    lists:foreach(fun(Line) ->
                          {ok, Reply0} = gen_tcp:recv(Socket, 0, 500),
                          Reply = binary:part(Reply0, 0, byte_size(Reply0) - 2),
                          io:format("~p~n", [Reply]),
                          ?assertEqual(Line, Reply)
                  end, Lines).
