-module(map_reduce_test).

-include_lib("eunit/include/eunit.hrl").

file5_test() ->
    Data = map_reduce:start(["data1.txt",
                             "data2.txt",
                             "data3.txt",
                             "data4.txt",
                             "data5.txt"]),
    ?assertEqual(3, maps:get("а", Data)),
    ?assertEqual(1, maps:get("бензопила", Data)),
    ?assertEqual(4, maps:get("в", Data)),
    ?assertEqual(2, maps:get("вслух", Data)),
    ?assertEqual(1, maps:get("заинтригован", Data)),
    ?assertEqual(1, maps:get("царя", Data)),
    ok.


file2_test() ->
    Data = map_reduce:start(["data1.txt", "data2.txt"]),
    ?assertEqual(2, maps:get("а", Data)),
    ?assertEqual(1, maps:get("бензопила", Data)),
    ?assertEqual(2, maps:get("в", Data)),
    ?assertEqual(error, maps:find("вслух", Data)),
    ?assertEqual(1, maps:get("заинтригован", Data)),
    ?assertEqual(1, maps:get("царя", Data)),
    ok.


file1_test() ->
    Data = map_reduce:start(["data1.txt", "data777.txt"]),
    ?assertEqual(error, maps:find("а", Data)),
    ?assertEqual(1, maps:get("бензопила", Data)),
    ?assertEqual(2, maps:get("в", Data)),
    ?assertEqual(error, maps:find("вслух", Data)),
    ?assertEqual(1, maps:get("заинтригован", Data)),
    ?assertEqual(error, maps:find("царя", Data)),
    ok.
