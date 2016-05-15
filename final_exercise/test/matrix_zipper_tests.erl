-module(matrix_zipper_tests).

-include_lib("eunit/include/eunit.hrl").


move_test() ->
    Z = matrix_zipper:from_matrix(
           [[ 1, 2, 3, 4],
            [ 5, 6, 7, 8],
            [ 9,10,11,12],
            [13,14,15,16]]
           ),
    Actions = [
               {get, 1},
               right, {get, 2},
               right, {get, 3},
               right, {get, 4},
               right, {get, 4},
               down, {get, 8},
               down, {get, 12},
               down, {get, 16},
               down, {get, 16},
               left, {get, 15},
               up, {get, 11},
               left, {get, 10},
               up, {get, 6},
               up, {get, 2},
               up, {get, 2},
               left, {get, 1},
               left, {get, 1}
              ],
    lists:foldl(fun check/2, Z, Actions),
    ok.


set_test() ->
    Z1 = matrix_zipper:from_matrix(
           [[ 1, 2, 3, 4],
            [ 5, 6, 7, 8],
            [ 9,10,11,12],
            [13,14,15,16]]
           ),
    Actions = [
               {set, a},
               down, {set, b},
               down, {set, c},
               down, {set, d},
               right, up, up, up, {set, aa},
               down, {set, bb},
               down, {set, cc},
               down, {set, dd},
               right, up, up, up, {set, aaa},
               right, {set, aaaa},
               down, left, {set, bbb},
               right, {set, bbbb},
               down, left, {set, ccc},
               right, {set, cccc},
               down, {set, dddd},
               left, {set, ddd}
              ],
    Z2 = lists:foldl(fun check/2, Z1, Actions),
    ?assertEqual(
           [[a, aa, aaa, aaaa],
            [b, bb, bbb, bbbb],
            [c, cc, ccc, cccc],
            [d, dd, ddd, dddd]],
       matrix_zipper:to_matrix(Z2)),
    ok.


position_test() ->
    Z = matrix_zipper:from_matrix(
           [[ 1, 2, 3, 4],
            [ 5, 6, 7, 8],
            [ 9,10,11,12],
            [13,14,15,16]]
           ),
    Actions = [
               {get, 1}, {pos, {1,1}},
               {right, 2}, {get, 3}, {pos, {3, 1}},
               {right, 2}, {get, 3}, {pos, {3, 1}}, % invalid move
               {right, 1}, {get, 4}, {pos, {4, 1}},
               {down, 2}, {get, 12}, {pos, {4, 3}},
               down, {get, 16}, {pos, {4, 4}},
               {down, 10}, {get, 16}, {pos, {4, 4}},
               {left, 1}, {get, 15}, {pos, {3, 4}},
               {up, 2}, {get, 7}, {pos, {3, 2}},
               {left, 10}, {get, 5}, {pos, {1, 2}}, % invalid move
               {up, 10}, {get, 5}, {pos, {1, 2}}, % invalid move
               {up, 1}, {get, 1}, {pos, {1, 1}}
              ],
    lists:foldl(fun check/2, Z, Actions),
    ok.


check({get, Res}, Z) ->
    ?assertEqual(Res, matrix_zipper:get(Z)),
    Z;
check({set, Val}, Z) ->
    matrix_zipper:set(Z, Val);
check({pos, Res}, Z) ->
    ?assertEqual(Res, matrix_zipper:position(Z)),
    Z;
check({Action, Arg}, Z) ->
    case matrix_zipper:Action(Z, Arg) of
        {ok, Z2} -> Z2;
        {error, _} -> Z
    end;
check(Action, Z) ->
    case matrix_zipper:Action(Z) of
        {ok, Z2} -> Z2;
        {error, _} -> Z
    end.
