-module(list_zipper_tests).

-include_lib("eunit/include/eunit.hrl").


main_test() ->
    Z0 = list_zipper:from_list([1,2,3,4,5]),
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
               {right, 3}, {get, 4},
               {set, 44}, {get, 44},
               right, {set, 55}, {get, 55},
               {left, 2}, {set, 33},
               left, {set, 22},
               left, {set, 11}
              ],
    Z2 = lists:foldl(fun({get, Res}, Z) ->
                             ?assertEqual(Res, list_zipper:get(Z)),
                             Z;
                        ({Action, Arg}, Z) ->
                             list_zipper:Action(Z, Arg);
                        (Action, Z) ->
                             list_zipper:Action(Z)
                     end, Z0, Actions),
    ?assertEqual([11,22,33,44,55], list_zipper:to_list(Z2)),
    ok.
