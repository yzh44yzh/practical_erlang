-module(main).

-export([get_users/0, get_females/1, split_by_age/1, get_id_name/1]).

get_users() ->
    [{user, 1, "Bob", male, 22},
     {user, 2, "Helen", female, 14},
     {user, 3, "Bill", male, 11},
     {user, 4, "Kate", female, 18}].



get_females(Users) -> get_females(Users, []).


get_females([], Acc) -> lists:reverse(Acc);

get_females([User | Rest], Acc) ->
    case User of
        {user, _, _, male, _} -> get_females(Rest, Acc);
        {user, _, _, female, _} -> get_females(Rest, [User | Acc])
    end.


split_by_age(Users) -> split_by_age(Users, {[], []}).


split_by_age([], {Acc1, Acc2}) -> {lists:reverse(Acc1), lists:reverse(Acc2)};

split_by_age([User | Rest], {Acc1, Acc2}) ->
    {user, _, _, _, Age} = User,
    if
        Age < 18 -> split_by_age(Rest, {[User | Acc1], Acc2});
        true -> split_by_age(Rest, {Acc1, [User | Acc2]})
    end.


get_id_name(Users) -> get_id_name(Users, []).


get_id_name([], Acc) -> lists:reverse(Acc);

get_id_name([User | Rest], Acc) ->
    {user, Id, Name, _, _} = User,
    get_id_name(Rest, [{Id, Name} | Acc]).
