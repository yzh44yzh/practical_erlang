-module(matrix_zipper).

-export([from_matrix/1, to_matrix/1,
         %% left/1, left/2,
         %% right/1, right/2,
         %% up/1, up/2,
         %% down/1, down/2,
         get/1, set/2
        ]).

-type matrix() :: [[any()]].
-type zrow() :: {pos_integer(), list_zipper:lz()}.
-type mz() :: list_zipper:lz().
-export_type([matrix/0, mz/0]).


%%% Module API

-spec from_matrix(matrix()) -> mz().
from_matrix(Matrix) ->
    ZRows = lists:map(fun list_to_zrow/1, Matrix),
    list_zipper:from_list(ZRows).


-spec to_matrix(mz()) -> matrix().
to_matrix(Zipper) ->
    ZRows = list_zipper:to_list(Zipper),
    lists:map(fun zrow_to_list/1, ZRows).


-spec get(mz()) -> any().
get(Zipper) ->
    {_Pos, RowZipper} = list_zipper:get(Zipper),
    list_zipper:get(RowZipper).


-spec set(mz(), any()) -> mz().
set(Zipper, Value) ->
    {Pos, RowZipper} = list_zipper:get(Zipper),
    RowZipper2 = list_zipper:set(RowZipper, Value),
    list_zipper:set(Zipper, {Pos, RowZipper2}).


%%% Inner Functions

-spec list_to_zrow(list()) -> zrow().
list_to_zrow(Row) -> {0, list_zipper:from_list(Row)}.


-spec zrow_to_list(zrow()) -> list().
zrow_to_list({_, Zipper}) -> list_zipper:to_list(Zipper).
