-module(ff_game_tests).

-include_lib("eunit/include/eunit.hrl").

find_player_test() ->
    F1 = ff_game:initial_field(5,5),
    ?assertEqual({1,1}, ff_game:find_player(player_a, F1)),
    ?assertEqual({5,5}, ff_game:find_player(player_b, F1)),

    F2 = ff_game:initial_field(10,8),
    ?assertEqual({1,1}, ff_game:find_player(player_a, F2)),
    ?assertEqual({8,10}, ff_game:find_player(player_b, F2)),

    F3 = [[stable, player_a, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, player_b, stable]],
    ?assertEqual({1,2}, ff_game:find_player(player_a, F3)),
    ?assertEqual({5,4}, ff_game:find_player(player_b, F3)),

    F4 = [[stable, stable, player_a, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, player_b, stable, stable]],
    ?assertEqual({1,3}, ff_game:find_player(player_a, F4)),
    ?assertEqual({5,3}, ff_game:find_player(player_b, F4)),

    F5 = [[stable, stable, stable, player_a, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, player_b, stable, stable, stable]],
    ?assertEqual({1,4}, ff_game:find_player(player_a, F5)),
    ?assertEqual({5,2}, ff_game:find_player(player_b, F5)),

    F6 = [[stable, stable, stable, stable, player_a],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [player_b, stable, stable, stable, stable]],
    ?assertEqual({1,5}, ff_game:find_player(player_a, F6)),
    ?assertEqual({5,1}, ff_game:find_player(player_b, F6)),

    F7 = [[stable, stable, stable, stable, stable],
          [player_a, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, player_b],
          [stable, stable, stable, stable, stable]],
    ?assertEqual({2,1}, ff_game:find_player(player_a, F7)),
    ?assertEqual({4,5}, ff_game:find_player(player_b, F7)),

    F8 = [[stable, stable, stable, stable, stable],
          [stable, stable, player_a, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, player_b, stable, stable],
          [stable, stable, stable, stable, stable]],
    ?assertEqual({2,3}, ff_game:find_player(player_a, F8)),
    ?assertEqual({4,3}, ff_game:find_player(player_b, F8)),

    F9 = [[stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, player_a, player_b, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable]],
    ?assertEqual({3,3}, ff_game:find_player(player_a, F9)),
    ?assertEqual({3,4}, ff_game:find_player(player_b, F9)),

    F10 = [[stable, stable, stable, stable, player_b],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, stable, stable],
          [stable, stable, stable, player_a, stable],
          [stable, stable, stable, stable, stable]],
    ?assertEqual({4,4}, ff_game:find_player(player_a, F10)),
    ?assertEqual({1,5}, ff_game:find_player(player_b, F10)),

    ok.
