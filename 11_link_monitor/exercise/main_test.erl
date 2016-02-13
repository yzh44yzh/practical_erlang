-module(main_test).

-include_lib("eunit/include/eunit.hrl").


main_test() ->
    ?assertMatch({#{<<"apples">> := 100,
                    <<"tomatos">> := 20,
                    <<"potato">> := 17,
                    <<"tangerin">> := 289,
                    <<"ananas">> := 14},
                  #{}},
                 main:parse(["data_1.csv"])),

    ?assertMatch({#{<<"melon">> := 332,
                    <<"cucumber">> := 12,
                    <<"tangerin">> := 23,
                    <<"pear">> := 52,
                    <<"apples">> := 120,
                    <<"potato">> := 77},
                  #{}},
                 main:parse(["data_2.csv"])),

    ?assertMatch({#{<<"ananas">> := 14,
                    <<"apples">> := 220,
                    <<"cucumber">> := 12,
                    <<"melon">> := 332,
                    <<"pear">> := 52,
                    <<"potato">> := 94,
                    <<"tangerin">> := 312,
                    <<"tomatos">> := 20},
                  #{}},
                 main:parse(["data_1.csv", "data_2.csv"])),

    ?assertMatch({#{},
                  #{"data_3.csv" := _}},
                 main:parse(["data_3.csv"])),

    ?assertMatch({#{},
                  #{"data_4.csv" := {{badmatch,{error,enoent}}, _}}},
                 main:parse(["data_4.csv"])),

    ?assertMatch({#{<<"ananas">> := 14,
                    <<"apples">> := 220,
                    <<"cucumber">> := 12,
                    <<"melon">> := 332,
                    <<"pear">> := 52,
                    <<"potato">> := 94,
                    <<"tangerin">> := 312,
                    <<"tomatos">> := 20},
                  #{"data_3.csv" := _}},
                 main:parse(["data_1.csv", "data_2.csv", "data_3.csv"])),

    ?assertMatch({#{<<"ananas">> := 14,
                    <<"apples">> := 220,
                    <<"cucumber">> := 12,
                    <<"melon">> := 332,
                    <<"pear">> := 52,
                    <<"potato">> := 94,
                    <<"tangerin">> := 312,
                    <<"tomatos">> := 20},
                  #{"data_3.csv" := _,
                    "data_4.csv" := {{badmatch,{error,enoent}}, _}}},
                 main:parse(["data_1.csv", "data_2.csv", "data_3.csv", "data_4.csv"])),
    ok.
