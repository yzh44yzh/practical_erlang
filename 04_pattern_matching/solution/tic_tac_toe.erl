-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win(GameState) ->
    case GameState of
        {{P, P, P}, _, _} when P =/= f -> {win, P};
        {_, {P, P, P}, _} when P =/= f -> {win, P};
        {_, _, {P, P, P}} when P =/= f -> {win, P};
        {{P, _, _},
         {P, _, _},
         {P, _, _}} when P =/= f -> {win, P};
        {{_, P, _},
         {_, P, _},
         {_, P, _}} when P =/= f -> {win, P};
        {{_, _, P},
         {_, _, P},
         {_, _, P}} when P =/= f -> {win, P};
        {{P, _, _},
         {_, P, _},
         {_, _, P}} when P =/= f -> {win, P};
        {{_, _, P},
         {_, P, _},
         {P, _, _}} when P =/= f -> {win, P};
        _ -> no_win
    end.


move(Cell, Player, GameState) ->
    case {Cell, GameState} of
        {1, {{f, C2, C3}, Row2, Row3}} -> {ok, {{Player, C2, C3}, Row2, Row3}};
        {2, {{C1, f, C3}, Row2, Row3}} -> {ok, {{C1, Player, C3}, Row2, Row3}};
        {3, {{C1, C2, f}, Row2, Row3}} -> {ok, {{C1, C2, Player}, Row2, Row3}};
        {4, {Row1, {f, C2, C3}, Row3}} -> {ok, {Row1, {Player, C2, C3}, Row3}};
        {5, {Row1, {C1, f, C3}, Row3}} -> {ok, {Row1, {C1, Player, C3}, Row3}};
        {6, {Row1, {C1, C2, f}, Row3}} -> {ok, {Row1, {C1, C2, Player}, Row3}};
        {7, {Row1, Row2, {f, C2, C3}}} -> {ok, {Row1, Row2, {Player, C2, C3}}};
        {8, {Row1, Row2, {C1, f, C3}}} -> {ok, {Row1, Row2, {C1, Player, C3}}};
        {9, {Row1, Row2, {C1, C2, f}}} -> {ok, {Row1, Row2, {C1, C2, Player}}};
        _ -> {error, invalid_move}
    end.
