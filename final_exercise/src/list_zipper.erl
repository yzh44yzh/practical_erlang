-module(list_zipper).

-export([from_list/1, to_list/1,
         left/1, left/2,
         right/1, right/2,
         get/1, set/2
        ]).

-type(list_zipper() :: {list(), term(), list()}).
-export_type([list_zipper/0]).


-spec from_list(list()) -> list_zipper().
from_list([H | T]) ->
    {[], H, T}.


-spec to_list(list_zipper()) -> list().
to_list({Left, Current, Right}) ->
    lists:reverse(Left) ++ [Current | Right].


-spec left(list_zipper()) -> list_zipper().
left({[], _, _} = Zipper) -> Zipper;
left({[H | Left], Current, Right}) ->
    {Left, H, [Current | Right]}.


-spec left(list_zipper(), pos_integer()) -> list_zipper().
left(Zipper, 0) -> Zipper;
left(Zipper, Steps) when Steps > 0 ->
    left(left(Zipper), Steps - 1).


-spec right(list_zipper()) -> list_zipper().
right({_, _, []} = Zipper) -> Zipper;
right({Left, Current, [H | Right]}) ->
    {[Current | Left], H, Right}.


-spec right(list_zipper(), pos_integer()) -> list_zipper().
right(Zipper, 0) -> Zipper;
right(Zipper, Steps) when Steps > 0 ->
    right(right(Zipper), Steps - 1).


-spec get(list_zipper()) -> term().
get({_, Current, _}) -> Current.


-spec set(list_zipper(), term()) -> term().
set({Left, _, Right}, Value) ->
    {Left, Value, Right}.
