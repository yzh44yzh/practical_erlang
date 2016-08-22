-module(task_3).

-export([distance/2]).

-include_lib("eunit/include/eunit.hrl").


%% Calculate distance between 2 points on 2D surface
%% points are given as tuple {X, Y}, distance should be float
distance({X1, Y1}, {X2, Y2}) ->
    0.0.

distance_test() ->
    ?assertEqual(5.0, distance({0, 0}, {0, 5})),
    ?assertEqual(5.0, distance({5, 0}, {0, 0})),
    ?assertEqual(0.0, distance({5, 5}, {5, 5})),
    ?assertEqual(5.0, distance({0, 0}, {3, 4})),
    ?assertEqual(5.0, distance({0, 0}, {-3, -4})),
    ?assertEqual(12.806248474865697, distance({2, 2}, {10, 12})),
    ?assertEqual(21.213203435596427, distance({-5, -5}, {10, 10})),
    ?assertEqual(21.400934559032695, distance({-5, 5}, {8, -12})),
    ?assertEqual(17.26267650163207, distance({-5, 5}, {-8, -12})),
    ok.
