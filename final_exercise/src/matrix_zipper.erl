-module(matrix_zipper).

-export([from_matrix/1, to_matrix/1,
         left/1, % left/2,
         right/1, % right/2,
         up/1, % up/2,
         down/1, % down/2,
         get/1, set/2
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


left(Zipper) ->
    move_in_curr_row(Zipper, left).


right(Zipper) ->
    move_in_curr_row(Zipper, right).


up(Zipper) ->
    change_row(Zipper, left).


down(Zipper) ->
    change_row(Zipper, right).


%%% Inner Functions

move_in_curr_row(Zipper, Direction) ->
    CurrRow = list_zipper:get(Zipper),
    CurrRow2 = list_zipper:Direction(CurrRow),
    list_zipper:set(Zipper, CurrRow2).


change_row(Zipper, Direction) ->
    OldRow = list_zipper:get(Zipper),
    OldRowPos = list_zipper:position(OldRow),
    Zipper2 = list_zipper:Direction(Zipper),
    NewRow = list_zipper:get(Zipper2),
    NewRowPos = list_zipper:position(NewRow),
    list_zipper:set(Zipper2,
                    if
                        NewRowPos == OldRowPos -> NewRow;
                        NewRowPos > OldRowPos -> list_zipper:left(NewRow, NewRowPos - OldRowPos);
                        NewRowPos < OldRowPos -> list_zipper:right(NewRow, OldRowPos - NewRowPos)
                    end).
