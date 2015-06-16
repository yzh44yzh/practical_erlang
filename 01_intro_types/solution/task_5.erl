-module(task_5).

-export([get_floats/1]).

-include_lib("eunit/include/eunit.hrl").


%% get 3 float numbers from binary
get_floats(Bin) ->
    <<F1/float, F2/float, F3/float>> = Bin,
    {F1, F2, F3}.


get_floats_test() ->
    ?assertEqual({1.0, 2.5, 100.500},
                 get_floats(<<63,240,0,0,0,0,0,0,
                              64,4,0,0,0,0,0,0,
                              64,89,32,0,0,0,0,0>>)),
    ?assertEqual({math:pi(), math:cos(2), math:log10(100)},
                 get_floats(<<64,9,33,251,84,68,45,24,
                              191,218,162,38,87,83,114,5,
                              64,0,0,0,0,0,0,0>>)),
    ok.
