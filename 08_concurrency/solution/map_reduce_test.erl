-module(map_reduce_test).

-include_lib("eunit/include/eunit.hrl").

file5_test() ->
    Data = map_reduce:start(["data1.txt",
                             "data2.txt",
                             "data3.txt",
                             "data4.txt",
                             "data5.txt"]),
    ?assertEqual(3, maps:get(<<"а"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"бензопила"/utf8>>, Data)),
    ?assertEqual(4, maps:get(<<"в"/utf8>>, Data)),
    ?assertEqual(2, maps:get(<<"вслух"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"заинтригован"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"царя"/utf8>>, Data)),
    ok.


file2_test() ->
    Data = map_reduce:start(["data1.txt", "data2.txt"]),
    ?assertEqual(2, maps:get(<<"а"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"бензопила"/utf8>>, Data)),
    ?assertEqual(2, maps:get(<<"в"/utf8>>, Data)),
    ?assertEqual(error, maps:find(<<"вслух"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"заинтригован"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"царя"/utf8>>, Data)),
    ok.


file1_test() ->
    Data = map_reduce:start(["data1.txt", "data777.txt"]),
    ?assertEqual(error, maps:find(<<"а"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"бензопила"/utf8>>, Data)),
    ?assertEqual(2, maps:get(<<"в"/utf8>>, Data)),
    ?assertEqual(error, maps:find(<<"вслух"/utf8>>, Data)),
    ?assertEqual(1, maps:get(<<"заинтригован"/utf8>>, Data)),
    ?assertEqual(error, maps:find(<<"царя"/utf8>>, Data)),
    ok.
