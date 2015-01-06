-module(my_lists).

-export([reverse/1]).

-include_lib("eunit/include/eunit.hrl").

%% TODO implement reverse/1

%% BEGIN
reverse(List) ->
    reverse(List, []).

reverse([], Acc) -> Acc;
reverse([H|T], Acc) -> reverse(T, [H|Acc]).
%% END

reverse_test() ->
    ?assertEqual([3,2,1], reverse([1,2,3])),
    ok.
