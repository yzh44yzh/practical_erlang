-module(task_2).

-export([is_equal/3]).

-include_lib("eunit/include/eunit.hrl").

%% Check are 2 float numbers equal with given precision
is_equal(F1, F2, Precision) ->
    abs(F1 - F2) < Precision.


is_equal_test() ->
    ?assertEqual(true, is_equal(3.5, 3.5, 0.01)),
    ?assertEqual(true, is_equal(3.51, 3.51, 0.01)),
    ?assertEqual(false, is_equal(3.51, 3.53, 0.01)),
    ?assertEqual(true, is_equal(3.51, 3.53, 0.1)),
    ?assertEqual(false, is_equal(3.501, 3.503, 0.001)),
    ?assertEqual(true, is_equal(3.501, 3.503, 0.01)),
    ?assertEqual(true, is_equal(-7.77, -7.75, 0.1)),
    ?assertEqual(true, is_equal(-10.95, -11.0, 0.2)),
    ?assertEqual(true, is_equal(-10.95, -11.0, 0.06)),
    ?assertEqual(false, is_equal(-10.95, -11.0, 0.02)),
    ok.
