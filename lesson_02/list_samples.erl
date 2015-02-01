-module(list_samples).

-export([filter_users_sample/0,
         get_names_sample/0,
         partition_sample/0
         ]).

filter_users_sample() ->
    Users = [{user, 1, "Bob", male},
             {user, 2, "Helen", female},
             {user, 3, "Bill", male},
             {user, 4, "Kate", female}],
    filter_female(Users, []).


filter_female([], Acc) -> Acc;
filter_female([User | Rest], Acc) ->
    case User of
        {user, _, _, male} -> filter_female(Rest, Acc);
        {user, _, _, female} -> filter_female(Rest, [User | Acc])
    end.


get_names_sample() ->
    Users = [{user, 1, "Bob", male},
             {user, 2, "Helen", female},
             {user, 3, "Bill", male},
             {user, 4, "Kate", female}],
    get_names(Users).

get_names(Users) -> get_names(Users, []).

get_names([], Acc) -> lists:reverse(Acc);
get_names([User | Rest], Acc) ->
    {user, Id, Name, _} = User,
    get_names(Rest, [{Id, Name} | Acc]).


partition_sample() ->
    Users = [{user, 1, "Bob", male, 27},
             {user, 2, "Helen", female, 18},
             {user, 3, "Bill", male, 15},
             {user, 4, "Kate", female, 11}],
    partition_users(Users).

partition_users(Users) -> partition_users(Users, {[], []}).

partition_users([], {List1, List2}) -> {lists:reverse(List1), lists:reverse(List2)};
partition_users([User | Rest], {List1, List2}) ->
    {user, _, _, _, Age} = User,
    if
        Age < 18 -> partition_users(Rest, {[User | List1], List2});
        true -> partition_users(Rest, {List1, [User | List2]})
    end.
