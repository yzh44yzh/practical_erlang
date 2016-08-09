-module(task_1).

-export([is_in_range/3]).

-include_lib("eunit/include/eunit.hrl").


%% Check is Val in range [From, To], both edges included
is_in_range(Val, Left, Right) ->
    Val >= Left andalso Val =< Right.


is_in_range_test() ->
    ?assertEqual(true, is_in_range(5, 2, 10)),
    ?assertEqual(true, is_in_range(5, 5, 10)),
    ?assertEqual(true, is_in_range(5, 1, 5)),
    ?assertEqual(false, is_in_range(50, 1, 5)),
    ?assertEqual(false, is_in_range(-5, 1, 5)),
    ?assertEqual(true, is_in_range(1, 1, 1)),
    ?assertEqual(false, is_in_range(1, 5, 1)),
    ok.
