-module(list_zipper).

-export([from_list/1, to_list/1,
         left/1, left/2,
         right/1, right/2,
         get/1, set/2, position/1
        ]).

-type lz() :: {[any()], any(), [any()], pos_integer()}.
-export_type([lz/0]).


-spec from_list(list()) -> lz().
from_list([H | T]) ->
    {[], H, T, 1}.


-spec to_list(lz()) -> list().
to_list({Left, Current, Right, _}) ->
    lists:reverse(Left) ++ [Current | Right].


-spec left(lz()) -> {ok, lz()} | {error, no_move}.
left({[], _, _, _}) -> {error, no_move};
left({[H | Left], Current, Right, Position}) ->
    {ok, {Left, H, [Current | Right], Position - 1}}.


-spec left(lz(), pos_integer()) -> {ok, lz()} | {error, no_move}.
left(Zipper, 0) -> {ok, Zipper};
left(Zipper, Steps) when Steps > 0 ->
    case left(Zipper) of
        {ok, Zipper2} -> left(Zipper2, Steps - 1);
        Error -> Error
    end.


-spec right(lz()) -> {ok, lz()} | {error, no_move}.
right({_, _, [], _}) -> {error, no_move};
right({Left, Current, [H | Right], Position}) ->
    {ok, {[Current | Left], H, Right, Position + 1}}.


-spec right(lz(), pos_integer()) -> {ok, lz()} | {error, no_move}.
right(Zipper, 0) -> {ok, Zipper};
right(Zipper, Steps) when Steps > 0 ->
    case right(Zipper) of
        {ok, Zipper2} -> right(Zipper2, Steps - 1);
        Error -> Error
    end.


-spec get(lz()) -> any().
get({_, Current, _, _}) -> Current.


-spec set(lz(), any()) -> any().
set({Left, _, Right, Position}, Value) ->
    {Left, Value, Right, Position}.


-spec position(lz()) -> pos_integer().
position({_, _, _, Position}) -> Position.
