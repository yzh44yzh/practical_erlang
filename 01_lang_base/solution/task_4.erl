-module(task_4).

-export([pack_floats/3]).

-include_lib("eunit/include/eunit.hrl").


%% pack 3 float number into single binary
pack_floats(F1, F2, F3) ->
    <<F1/float, F2/float, F3/float>>.


pack_floats_test() ->
    ?assertEqual(<<63,240,0,0,0,0,0,0,
                   64,4,0,0,0,0,0,0,
                   64,89,32,0,0,0,0,0>>,
                 pack_floats(1.0, 2.5, 100.500)),
    ?assertEqual(<<64,9,33,251,84,68,45,24,
                   191,218,162,38,87,83,114,5,
                   64,0,0,0,0,0,0,0>>,
                   pack_floats(math:pi(), math:cos(2), math:log10(100))),
    ok.
