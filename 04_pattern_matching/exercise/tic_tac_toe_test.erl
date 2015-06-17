-module(tic_tac_toe_test).

-include_lib("eunit/include/eunit.hrl").

win_test() ->
    ?assertEqual(no_win, tic_tac_toe:win(tic_tac_toe:new_game())),
    ?assertEqual(no_win, tic_tac_toe:win({{x,o,x},
                                          {o,x,o},
                                          {o,x,o}})),
    ?assertEqual({win, x}, tic_tac_toe:win({{x,x,x},
                                            {f,f,f},
                                            {f,f,f}})),
    ?assertEqual({win, o}, tic_tac_toe:win({{f,f,x},
                                            {o,o,o},
                                            {f,x,f}})),
    ?assertEqual({win, x}, tic_tac_toe:win({{o,o,f},
                                            {f,o,f},
                                            {x,x,x}})),
    ?assertEqual({win, o}, tic_tac_toe:win({{o,f,f},
                                            {o,x,f},
                                            {o,x,x}})),
    ?assertEqual({win, x}, tic_tac_toe:win({{f,x,f},
                                            {o,x,o},
                                            {f,x,o}})),
    ?assertEqual({win, o}, tic_tac_toe:win({{x,x,o},
                                            {o,x,o},
                                            {x,f,o}})),
    ?assertEqual({win, x}, tic_tac_toe:win({{x,f,o},
                                            {f,x,o},
                                            {f,o,x}})),
    ?assertEqual({win, o}, tic_tac_toe:win({{x,x,o},
                                            {x,o,x},
                                            {o,f,f}})),
    ok.


move_test() ->
    G0 = tic_tac_toe:new_game(),

    {ok, G1} = tic_tac_toe:move(1, x, G0),
    ?assertEqual({{x,f,f},{f,f,f},{f,f,f}}, G1),
    ?assertEqual({error, invalid_move}, tic_tac_toe:move(1, x, G1)),

    {ok, G2} = tic_tac_toe:move(5, o, G1),
    ?assertEqual({{x,f,f},{f,o,f},{f,f,f}}, G2),
    ?assertEqual({error, invalid_move}, tic_tac_toe:move(5, o, G2)),

    {ok, G3} = tic_tac_toe:move(4, x, G2),
    ?assertEqual({{x,f,f},{x,o,f},{f,f,f}}, G3),
    ?assertEqual({error, invalid_move}, tic_tac_toe:move(4, x, G3)),

    {ok, G4} = tic_tac_toe:move(7, o, G3),
    ?assertEqual({{x,f,f},{x,o,f},{o,f,f}}, G4),
    ?assertEqual({error, invalid_move}, tic_tac_toe:move(7, o, G4)),

    {ok, G5} = tic_tac_toe:move(2, x, G4),
    ?assertEqual({{x,x,f},{x,o,f},{o,f,f}}, G5),
    ?assertEqual({error, invalid_move}, tic_tac_toe:move(2, x, G5)),

    {ok, G6} = tic_tac_toe:move(3, o, G5),
    ?assertEqual({{x,x,o},{x,o,f},{o,f,f}}, G6),
    ?assertEqual({error, invalid_move}, tic_tac_toe:move(3, o, G6)),

    ?assertEqual({win, o}, tic_tac_toe:win(G6)),

    ok.


move2_test() ->
    ?assertEqual({ok, {{x,f,f},{f,f,f},{f,f,f}}}, tic_tac_toe:move(1, x, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,x,f},{f,f,f},{f,f,f}}}, tic_tac_toe:move(2, x, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,f,x},{f,f,f},{f,f,f}}}, tic_tac_toe:move(3, x, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,f,f},{x,f,f},{f,f,f}}}, tic_tac_toe:move(4, x, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,f,f},{f,x,f},{f,f,f}}}, tic_tac_toe:move(5, x, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,f,f},{f,f,o},{f,f,f}}}, tic_tac_toe:move(6, o, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,f,f},{f,f,f},{o,f,f}}}, tic_tac_toe:move(7, o, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,f,f},{f,f,f},{f,o,f}}}, tic_tac_toe:move(8, o, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({ok, {{f,f,f},{f,f,f},{f,f,o}}}, tic_tac_toe:move(9, o, {{f,f,f},{f,f,f},{f,f,f}})),
    ?assertEqual({error, invalid_move}, tic_tac_toe:move(10, o, {{f,f,f},{f,f,f},{f,f,f}})),
    ok.
