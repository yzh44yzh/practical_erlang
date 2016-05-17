-module(list_zipper).

-export([from_list/1, to_list/1,
         left/1, left/2,
         right/1, right/2,
         get/1, set/2, position/1,
         find/2, find_left/2, find_right/2
        ]).

-type lz() :: {[any()], any(), [any()], pos_integer()}.
-export_type([lz/0]).


%%% Module API

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


-spec find(lz(), any()) -> {ok, lz()} | {error, not_found}.
find(Zipper, Value) ->
    {ok, Begin} = left(Zipper, position(Zipper) - 1),
    case list_zipper:get(Begin) of
        Value -> {ok, Begin};
        _ -> find_right(Begin, Value)
    end.


-spec find_right(lz(), any()) -> {ok, lz()} | {error, not_found}.
find_right(Zipper, Value) ->
    find_direction(Zipper, Value, right).


-spec find_left(lz(), any()) -> {ok, lz()} | {error, not_found}.
find_left(Zipper, Value) ->
    find_direction(Zipper, Value, left).


%%% Inner Functions

-spec find_direction(lz(), any(), left | right) -> {ok, lz()} | {error, not_found}.
find_direction(Zipper, Value, Direction) ->
    case list_zipper:Direction(Zipper) of
        {ok, Zipper2} ->
            case list_zipper:get(Zipper2) of
                Value -> {ok, Zipper2};
                _ -> find_direction(Zipper2, Value, Direction)
            end;
        {error, no_move} -> {error, not_found}
    end.
