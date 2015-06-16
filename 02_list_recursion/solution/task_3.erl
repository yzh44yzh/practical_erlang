-module(task_3).

-export([member/2, filter/2]).

-include_lib("eunit/include/eunit.hrl").


%% implement lists:member/2
%% http://www.erlang.org/doc/man/lists.html#member-2
member(Elem, List) ->
    case List of
        [] -> false;
        [Elem | _] -> true;
        [_ | Tail] -> member(Elem, Tail)
    end.


member_test() ->
    ?assertEqual(true, member(55, [1,2,55,77])),
    ?assertEqual(false, member(55, [])),
    ?assertEqual(false, member(55, [1,2,77])),
    ?assertEqual(true, member("ab", ["dd", "bd", "ab"])),
    ok.



%% implement lists:filter/2
%% http://www.erlang.org/doc/man/lists.html#filter-2
filter(Pred, List) ->
    filter(Pred, List, []).

filter(_Pred, [], Acc) -> task_2:reverse(Acc);
filter(Pred, [Head | Tail], Acc) ->
    case Pred(Head) of
        true -> filter(Pred, Tail, [Head | Acc]);
        false -> filter(Pred, Tail, Acc)
    end.


filter_test() ->
    F = fun(Val) -> Val rem 5 =:= 0 end,
    ?assertEqual([], filter(F, [])),
    ?assertEqual([], filter(F, [1,2,3,4])),
    ?assertEqual([5,10], filter(F, [1,2,3,4,5,6,7,8,9,10])),
    ok.
