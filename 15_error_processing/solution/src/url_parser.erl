-module(url_parser).

-export([parse/1]).


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    Res = pipeline(URL, #{},
            [
                fun get_protocol/2,
                fun get_domain/2,
                fun get_query/2,
                fun get_path/2,
                fun get_date/2
            ]),
    case Res of
        {error, Reason} -> {error, Reason};
        {_, Data} -> {ok, Data}
    end.


pipeline(Data, State, Funs) ->
    lists:foldl(
        fun
            (_Fun, {error, Reason}) -> {error, Reason};
            (Fun, {D, S}) ->
                case Fun(D, S) of
                    {ok, D2, S2} -> {D2,S2};
                    {error, Reason} -> {error, Reason}
                end
        end,
        {Data, State},
        Funs).


get_protocol(Data, State) ->
    case binary:split(Data, <<"://">>) of
        [Protocol, Rest] -> {ok, Rest, State#{protocol => Protocol}};
        _ -> {error, invalid_protocol}
    end.


get_domain(Data, State) ->
    case binary:split(Data, <<"/">>) of
        [<<>>] -> {error, invalid_domain};
        [Domain] -> {ok, <<>>, State#{domain => Domain}};
        [Domain, Rest] -> {ok, Rest, State#{domain => Domain}}
    end.


get_query(Data, State) ->
    case binary:split(Data, <<"?">>) of
        [Rest] -> {ok, Rest, State#{query => <<>>}};
        [Rest, Query] -> {ok, Rest, State#{query => Query}}
    end.


get_path(Data, State) ->
    Path = binary:split(Data, <<"/">>, [global]),
    Path2 = lists:filter(fun(P) -> P =/= <<>> end, Path),
    {ok, <<>>, State#{path => Path2}}.


get_date(Data, #{path := Path} = State) ->
    Date = case Path of
               [Y, M, D | _] ->
                   Res = pipeline([Y, M, D], [],
                                  [
                                   fun get_int/2,
                                   fun get_int/2,
                                   fun get_int/2,
                                   fun validate_date/2
                                  ]),
                   case Res of
                       {[], Dt} -> Dt;
                       {error, _} -> undefined
                   end;
               _ -> undefined
           end,
    {ok, Data, State#{date => Date}}.


get_int([Data | Rest], State) ->
    case string:to_integer(binary_to_list(Data)) of
        {Int, []} -> {ok, Rest, [Int | State]};
        _ -> {error, not_int}
    end.


validate_date(_, [D, M, Y]) when D >= 1, D =< 31, M >= 1, M =< 12 -> {ok, [], {Y, M, D}};
validate_date(_, _) -> {error, invalid_date}.
