-module(list_zipper).

-export([from_list/1, to_list/1,
         left/1, left/2,
         right/1, right/2,
         get/1, set/2
        ]).

-type lz() :: {[any()], any(), [any()]}.
-export_type([lz/0]).


-spec from_list(list()) -> lz().
from_list([H | T]) ->
    {[], H, T}.


-spec to_list(lz()) -> list().
to_list({Left, Current, Right}) ->
    lists:reverse(Left) ++ [Current | Right].


-spec left(lz()) -> lz().
left({[], _, _} = Zipper) -> Zipper;
left({[H | Left], Current, Right}) ->
    {Left, H, [Current | Right]}.


-spec left(lz(), pos_integer()) -> lz().
left(Zipper, 0) -> Zipper;
left(Zipper, Steps) when Steps > 0 ->
    left(left(Zipper), Steps - 1).


-spec right(lz()) -> lz().
right({_, _, []} = Zipper) -> Zipper;
right({Left, Current, [H | Right]}) ->
    {[Current | Left], H, Right}.


-spec right(lz(), pos_integer()) -> lz().
right(Zipper, 0) -> Zipper;
right(Zipper, Steps) when Steps > 0 ->
    right(right(Zipper), Steps - 1).


-spec get(lz()) -> any().
get({_, Current, _}) -> Current.


-spec set(lz(), any()) -> any().
set({Left, _, Right}, Value) ->
    {Left, Value, Right}.
