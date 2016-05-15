-module(list_zipper_tests).

-include_lib("eunit/include/eunit.hrl").


move_test() ->
    Z = list_zipper:from_list([1,2,3,4,5]),
    Actions = [
               {get, 1},
               right, {get, 2},
               right, {get, 3},
               right, {get, 4},
               right, {get, 5},
               right, {get, 5},
               {left, 3}, {get, 2},
               left, {get, 1},
               left, {get, 1},
               {right, 3}, {get, 4}
              ],
    lists:foldl(fun check/2, Z, Actions),
    ok.


set_test() ->
    Z1 = list_zipper:from_list([1,2,3,4,5]),
    Actions = [
               {right, 3}, {get, 4},
               {set, 44}, {get, 44},
               right, {set, 55}, {get, 55},
               {left, 2}, {set, 33},
               left, {set, 22},
               left, {set, 11}
              ],
    Z2 = lists:foldl(fun check/2, Z1, Actions),
    ?assertEqual([11,22,33,44,55], list_zipper:to_list(Z2)),
    ok.


position_test() ->
    Z = list_zipper:from_list([1,2,3,4,5]),
    Actions = [
               {pos, 1},
               right, {pos, 2},
               right, {pos, 3},
               right, {pos, 4},
               right, {pos, 5},
               right, {pos, 5},
               left, {pos, 4},
               left, {pos, 3},
               left, {pos, 2},
               left, {pos, 1},
               left, {pos, 1},
               {right, 2}, {pos, 3},
               {right, 10}, {pos, 3},
               {right, 2}, {pos, 5},
               {left, 4}, {pos, 1},
               {right, 2}, {pos, 3},
               {left, 10}, {pos, 3}
              ],
    lists:foldl(fun check/2, Z, Actions),
    ok.


find_test() ->
    Z = list_zipper:from_list([1,2,3,4,5,4,3,2,1]),
    {error, not_found} = list_zipper:find(Z, 10),
    {ok, Z2} = list_zipper:find(Z, 3),
    ?assertEqual(3, list_zipper:get(Z2)),
    {ok, Z3} = list_zipper:find_right(Z2, 3),
    ?assertEqual(3, list_zipper:get(Z3)),
    ?assertNotEqual(Z2, Z3),
    {error, not_found} = list_zipper:find_right(Z3, 3),
    {ok, Z4} = list_zipper:find_left(Z3, 3),
    ?assertEqual(Z2, Z4),
    ok.


check({get, Res}, Z) ->
    ?assertEqual(Res, list_zipper:get(Z)),
    Z;
check({set, Val}, Z) ->
    list_zipper:set(Z, Val);
check({pos, Res}, Z) ->
    ?assertEqual(Res, list_zipper:position(Z)),
    Z;
check({Action, Arg}, Z) ->
    case list_zipper:Action(Z, Arg) of
        {ok, Z2} -> Z2;
        {error, _} -> Z
    end;
check(Action, Z) ->
    case list_zipper:Action(Z) of
        {ok, Z2} -> Z2;
        {error, _} -> Z
    end.
