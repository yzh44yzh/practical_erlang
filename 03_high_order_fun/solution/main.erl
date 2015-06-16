-module(main).

-export([sample_champ/0, get_stat/1, filter_sick_players/1, make_pairs/2]).
-include_lib("eunit/include/eunit.hrl").


sample_champ() ->
    [
     {team, "Crazy Bulls",
      [{player, "Big Bull",        22, 545, 99},
       {player, "Small Bull",      18, 324, 95},
       {player, "Bull Bob",        19,  32, 45},
       {player, "Bill The Bull",   23, 132, 85},
       {player, "Tall Ball Bull",  38,  50, 50},
       {player, "Bull Dog",        35, 201, 91},
       {player, "Bull Tool",       29,  77, 96},
       {player, "Mighty Bull",     22, 145, 98}
      ]},
     {team, "Cool Horses",
      [{player, "Lazy Horse",      21, 423, 80},
       {player, "Sleepy Horse",    23, 101, 35},
       {player, "Horse Doors",     19,  87, 23},
       {player, "Rainbow",         21, 200, 17},
       {player, "HoHoHorse",       20, 182, 44},
       {player, "Pony",            25,  96, 76},
       {player, "Hippo",           17, 111, 96},
       {player, "Hop-Hop",         31, 124, 49}
      ]},
     {team, "Fast Cows",
      [{player, "Flash Cow",       18,  56, 34},
       {player, "Cow Bow",         28,  89, 90},
       {player, "Boom! Cow",       20, 131, 99},
       {player, "Light Speed Cow", 21, 201, 98},
       {player, "Big Horn",        23,  38, 93},
       {player, "Milky",           25,  92, 95},
       {player, "Jumping Cow",     19, 400, 98},
       {player, "Cow Flow",        18, 328, 47}
      ]},
     {team, "Fury Hens",
      [{player, "Ben The Hen",     57, 403, 83},
       {player, "Hen Hen",         20, 301, 56},
       {player, "Son of Hen",      21, 499, 43},
       {player, "Beak",            22,  35, 96},
       {player, "Superhen",        27,  12, 26},
       {player, "Henman",          20,  76, 38},
       {player, "Farm Hen",        18, 131, 47},
       {player, "Henwood",         40, 198, 77}
      ]},
     {team, "Extinct Mosters",
      [{player, "T-Rex",           21, 999, 99},
       {player, "Velociraptor",    29, 656, 99},
       {player, "Giant Mammoth",   30, 382, 99},
       {player, "The Big Croc",    42, 632, 99},
       {player, "Huge Pig",        18, 125, 98},
       {player, "Saber-Tooth",     19, 767, 97},
       {player, "Beer Bear",       24, 241, 99},
       {player, "Pure Horror",     31,  90, 43}
      ]}
    ].


get_stat(Champ) ->
    TeamsStat = lists:map(fun get_team_stat/1, Champ),
    {TotalPlayers, TotalAge, TotalRating} =
        lists:foldl(fun({TeamPlayers, TeamAge, TeamRating}, {P, A, R}) ->
                            {P + TeamPlayers, A + TeamAge, R + TeamRating}
                    end, {0, 0, 0}, TeamsStat),
    {length(Champ), TotalPlayers, TotalAge / TotalPlayers, TotalRating / TotalPlayers}.


get_team_stat({team, _, Players}) ->
    {TeamAge, TeamRating} =
        lists:foldl(
          fun({player, _, Age, Rating, _}, {TotalAge, TotalRating}) ->
                  {TotalAge + Age, TotalRating + Rating}
          end, {0, 0}, Players),
    {length(Players), TeamAge, TeamRating}.


get_stat_test() ->
    ?assertEqual({5,40,24.85,242.8}, get_stat(sample_champ())),
    ok.


filter_sick_players(Champ) ->
    HealthyTeams = lists:map(fun filter_team/1, Champ),
    lists:filter(fun({team, _, Players}) -> length(Players) >= 5 end, HealthyTeams).

filter_team({team, Name, Players}) ->
    HealthyPlayers = lists:filter(fun({player, _, _, _, Health}) -> Health >= 50 end, Players),
    {team, Name, HealthyPlayers}.


filter_sick_players_test() ->
    Result = [{team, "Crazy Bulls",
               [{player, "Big Bull",        22, 545, 99},
                {player, "Small Bull",      18, 324, 95},
                {player, "Bill The Bull",   23, 132, 85},
                {player, "Tall Ball Bull",  38,  50, 50},
                {player, "Bull Dog",        35, 201, 91},
                {player, "Bull Tool",       29,  77, 96},
                {player, "Mighty Bull",     22, 145, 98}
               ]},
              {team, "Fast Cows",
               [{player, "Cow Bow",         28,  89, 90},
                {player, "Boom! Cow",       20, 131, 99},
                {player, "Light Speed Cow", 21, 201, 98},
                {player, "Big Horn",        23,  38, 93},
                {player, "Milky",           25,  92, 95},
                {player, "Jumping Cow",     19, 400, 98}
               ]},
              {team, "Extinct Mosters",
               [{player, "T-Rex",           21, 999, 99},
                {player, "Velociraptor",    29, 656, 99},
                {player, "Giant Mammoth",   30, 382, 99},
                {player, "The Big Croc",    42, 632, 99},
                {player, "Huge Pig",        18, 125, 98},
                {player, "Saber-Tooth",     19, 767, 97},
                {player, "Beer Bear",       24, 241, 99}
               ]}
             ],
    ?assertEqual(Result, filter_sick_players(sample_champ())),
    ok.


make_pairs(Team1, Team2) ->
    {team, _, Players1} = Team1,
    {team, _, Players2} = Team2,
    [{Name1, Name2} || {player, Name1, _, Rating1, _} <- Players1,
                       {player, Name2, _, Rating2, _} <- Players2,
                       Rating1 + Rating2 > 600].


make_pairs_test() ->
    [T1, T2, T3, T4, _] = sample_champ(),
    ?assertEqual([{"Big Bull","Lazy Horse"},
                  {"Big Bull","Sleepy Horse"},
                  {"Big Bull","Horse Doors"},
                  {"Big Bull","Rainbow"},
                  {"Big Bull","HoHoHorse"},
                  {"Big Bull","Pony"},
                  {"Big Bull","Hippo"},
                  {"Big Bull","Hop-Hop"},
                  {"Small Bull","Lazy Horse"},
                  {"Bull Dog","Lazy Horse"}],
                 main:make_pairs(T1, T2)),
    ?assertEqual([{"Lazy Horse","Light Speed Cow"},
                  {"Lazy Horse","Jumping Cow"},
                  {"Lazy Horse","Cow Flow"}],
                 main:make_pairs(T2, T3)),
    ?assertEqual([{"Ben The Hen","Light Speed Cow"},
                  {"Ben The Hen","Jumping Cow"},
                  {"Ben The Hen","Cow Flow"},
                  {"Hen Hen","Jumping Cow"},
                  {"Hen Hen","Cow Flow"},
                  {"Son of Hen","Boom! Cow"},
                  {"Son of Hen","Light Speed Cow"},
                  {"Son of Hen","Jumping Cow"},
                  {"Son of Hen","Cow Flow"}],
                 main:make_pairs(T4, T3)),
    ok.
