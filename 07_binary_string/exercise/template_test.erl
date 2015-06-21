-module(template_test).

-include_lib("eunit/include/eunit.hrl").


parse_test() ->
    In = <<"hello {{name}}!">>,
    Out = <<"hello Bob!">>,
    Data = #{<<"name">> => <<"Bob">>},
    ?assertEqual(Out, template:parse(In, Data)),
    ok.


parse_many_params_test() ->
    In = <<"User {{name}} won {{wins}} games and got {{points}} points">>,
    Out = <<"User Kate won 55 games and got 777 points">>,
    Data = #{<<"name">> => "Kate",
              <<"wins">> => 55,
              <<"points">> => 777},
    ?assertEqual(Out, template:parse(In, Data)),
    ok.


param_at_first_or_last_position_test() ->
    In1 = <<"{{name}} rocks!">>,
    Out1 = <<"Bill rocks!">>,
    Data1 = #{<<"name">> => <<"Bill">>},
    ?assertEqual(Out1, template:parse(In1, Data1)),

    In2 = <<"watch your {{direction}}">>,
    Out2 = <<"watch your back">>,
    Data2 = #{<<"direction">> => "back"},
    ?assertEqual(Out2, template:parse(In2, Data2)),

    In3 = <<"{{user}}, watch your {{direction}}">>,
    Out3 = <<"Bob, watch your back">>,
    Data3 = #{<<"user">> => <<"Bob">>, <<"direction">> => "back"},
    ?assertEqual(Out3, template:parse(In3, Data3)),
    ok.


no_params_in_str_test() ->
    In = <<"no params in this string">>,
    Out = <<"no params in this string">>,
    Data = #{<<"param1">> => <<"value1">>, <<"param2">> => <<"value1">>},
    ?assertEqual(Out, template:parse(In, Data)),
    ok.


no_params_in_data_test() ->
    In = <<"quick brown {{animal1}} jump over lazy {{animal2}}">>,
    Out = <<"quick brown  jump over lazy ">>,
    Data = #{<<"param1">> => <<"value1">>, <<"param2">> => <<"value1">>},
    ?assertEqual(Out, template:parse(In, Data)),

    ok.
