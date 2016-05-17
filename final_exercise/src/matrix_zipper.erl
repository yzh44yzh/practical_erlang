-module(matrix_zipper).

-export([from_matrix/1, to_matrix/1,
         left/1, left/2,
         right/1, right/2,
         up/1, up/2,
         down/1, down/2,
         get/1, set/2,
         position/1,
         find/2, find_left/2, find_right/2, find_up/2, find_down/2
        ]).

-type matrix() :: [[any()]].
-type mz() :: list_zipper:lz().
-export_type([matrix/0, mz/0]).


%%% Module API

-spec from_matrix(matrix()) -> mz().
from_matrix(Matrix) ->
    Rows = lists:map(fun list_zipper:from_list/1, Matrix),
    list_zipper:from_list(Rows).


-spec to_matrix(mz()) -> matrix().
to_matrix(Zipper) ->
    Rows = list_zipper:to_list(Zipper),
    lists:map(fun list_zipper:to_list/1, Rows).


-spec get(mz()) -> any().
get(Zipper) ->
    CurrRow = list_zipper:get(Zipper),
    list_zipper:get(CurrRow).


-spec set(mz(), any()) -> mz().
set(Zipper, Value) ->
    CurrRow = list_zipper:get(Zipper),
    CurrRow2 = list_zipper:set(CurrRow, Value),
    list_zipper:set(Zipper, CurrRow2).


-spec left(mz()) -> {ok, mz()} | {error, no_move}.
left(Zipper) ->
    move_in_curr_row(Zipper, left, 1).


-spec left(mz(), pos_integer()) -> {ok, mz()} | {error, no_move}.
left(Zipper, Steps) ->
    move_in_curr_row(Zipper, left, Steps).


-spec right(mz()) -> {ok, mz()} | {error, no_move}.
right(Zipper) ->
    move_in_curr_row(Zipper, right, 1).


-spec right(mz(), pos_integer()) -> {ok, mz()} | {error, no_move}.
right(Zipper, Steps) ->
    move_in_curr_row(Zipper, right, Steps).


-spec up(mz()) -> {ok, mz()} | {error, no_move}.
up(Zipper) ->
    change_row(Zipper, left, 1).


-spec up(mz(), pos_integer()) -> {ok, mz()} | {error, no_move}.
up(Zipper, Steps) ->
    change_row(Zipper, left, Steps).


-spec down(mz()) -> {ok, mz()} | {error, no_move}.
down(Zipper) ->
    change_row(Zipper, right, 1).


-spec down(mz(), pos_integer()) -> {ok, mz()} | {error, no_move}.
down(Zipper, Steps) ->
    change_row(Zipper, right, Steps).


-spec position(mz()) -> {pos_integer(), pos_integer()}.
position(Zipper) ->
    CurrRow = list_zipper:get(Zipper),
    {list_zipper:position(CurrRow), list_zipper:position(Zipper)}.


-spec find(mz(), term()) -> {ok, mz()} | {error, not_found}.
find(Zipper, Value) ->
    {ok, Begin} = up(Zipper, list_zipper:position(Zipper) - 1),
    case find_curr_row(Begin, Value, find) of
        {ok, Zipper2} -> {ok, Zipper2};
        {error, not_found} -> find_next_row(Begin, Value)
    end.


-spec find_left(mz(), term()) -> {ok, mz()} | {error, not_found}.
find_left(Zipper, Value) ->
    find_curr_row(Zipper, Value, find_left).


-spec find_right(mz(), term()) -> {ok, mz()} | {error, not_found}.
find_right(Zipper, Value) ->
    find_curr_row(Zipper, Value, find_right).


-spec find_up(mz(), term()) -> {ok, mz()} | {error, not_found}.
find_up(Zipper, Value) ->
    find_direction(Zipper, Value, up, find_up).


-spec find_down(mz(), term()) -> {ok, mz()} | {error, not_found}.
find_down(Zipper, Value) ->
    find_direction(Zipper, Value, down, find_down).


%%% Inner Functions

move_in_curr_row(Zipper, Direction, Steps) ->
    CurrRow = list_zipper:get(Zipper),
    case list_zipper:Direction(CurrRow, Steps) of
        {ok, CurrRow2} -> {ok, list_zipper:set(Zipper, CurrRow2)};
        Error -> Error
    end.


change_row(Zipper, Direction, Steps) ->
    OldRow = list_zipper:get(Zipper),
    OldRowPos = list_zipper:position(OldRow),
    case list_zipper:Direction(Zipper, Steps) of
        {ok, Zipper2} ->
            NewRow = list_zipper:get(Zipper2),
            NewRowPos = list_zipper:position(NewRow),
            Zipper3 = list_zipper:set(Zipper2,
                            if
                                NewRowPos == OldRowPos -> NewRow;
                                NewRowPos > OldRowPos ->
                                    {ok, NewRow2} = list_zipper:left(NewRow, NewRowPos - OldRowPos),
                                    NewRow2;
                                NewRowPos < OldRowPos ->
                                    {ok, NewRow2} = list_zipper:right(NewRow, OldRowPos - NewRowPos),
                                    NewRow2
                            end),
            {ok, Zipper3};
        Error -> Error
    end.


find_curr_row(Zipper, Value, Fun) ->
    CurrRow = list_zipper:get(Zipper),
    case list_zipper:Fun(CurrRow, Value) of
        {ok, CurrRow2} -> {ok, list_zipper:set(Zipper, CurrRow2)};
        Error -> Error
    end.


find_next_row(Zipper, Value) ->
    case down(Zipper) of
        {ok, Zipper2} ->
            case find_curr_row(Zipper2, Value, find) of
                {ok, Zipper3} -> {ok, Zipper3};
                {error, not_found} -> find_next_row(Zipper2, Value)
            end;
        {error, no_move} -> {error, not_found}
    end.


find_direction(Zipper, Value, MoveDirection, FindDirection) ->
    CurrRow = list_zipper:get(Zipper),
    case list_zipper:get(CurrRow) of
        Value -> {ok, Zipper};
        _ -> case ?MODULE:MoveDirection(Zipper) of
                 {ok, Zipper2} -> ?MODULE:FindDirection(Zipper2, Value);
                 {error, no_move} -> {error, not_found}
             end
    end.
