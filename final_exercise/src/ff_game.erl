-module(ff_game).

-export([test/0]).
-export([initial_field/0, initial_field/2, draw_field/1]).

-type(cell() :: stable | fallen | player_a | player_b).
-type(row() :: [cell()]).
-type(field() :: [row()]).
-type(size() :: 5..500).


test() ->
    B = draw_field(initial_field()),
    io:format("~s", [B]),
    ok.


%%% Module API

-spec initial_field() -> field().
initial_field() ->
    initial_field(10, 10).


-spec initial_field(size(), size()) -> field().
initial_field(W, H) when W >= 5 andalso H >= 5 ->
    [first_row(W) |
     [row(W) || _ <- lists:seq(1, H - 2)]
     ++ [last_row(W)]].


-spec draw_field(field()) -> binary().
draw_field(Field) ->
    unicode:characters_to_binary(
      [
       draw_underline(length(hd(Field))),
       lists:map(fun draw_row/1, Field)
      ]).


%%% Inner Functions

-spec first_row(size()) -> row().
first_row(Size) ->
    [player_a | row(Size - 1)].


-spec last_row(size()) -> row().
last_row(Size) ->
    row(Size - 1) ++ [player_b].


-spec row(size()) -> row().
row(Size) ->
    [stable || _ <- lists:seq(1, Size)].


-spec draw_row(row()) -> iolist().
draw_row(Row) ->
    R = lists:map(fun draw_cell/1, Row),
    U = draw_underline(length(R)),
    [R, "|\r\n", U].


-spec draw_underline(size()) -> iolist().
draw_underline(Size) ->
    [[" ---" || _ <- lists:seq(1, Size)], "\r\n"].


-spec draw_cell(cell()) -> iolist().
draw_cell(stable)   -> "|   ";
draw_cell(fallen)   -> "| x ";
draw_cell(player_a) -> "| A ";
draw_cell(player_b) -> "| B ".
