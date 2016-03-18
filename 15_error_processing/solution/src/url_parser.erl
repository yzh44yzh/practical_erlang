-module(url_parser).

-export([test/0, parse/1]).

test() ->
    URLs = [
        <<"https://www.youtube.com/user/ErlangSolutions/playlists">>,
        <<"https://www.youtube.com/playlist?list=PLWbHc_FXPo2jN8cLhwLg7frCx6fJ-GPwM">>,
        <<"http://mostlyerlang.com/2015/05/12/067-rebar-3/">>,
        <<"http://mostlyerlang.com/2015/05/12/067-rebar-3?get">>,
        <<"http://mostlyerlang.com/2015/05/12/067-rebar-3?get?a=b?c=d">>,
        <<"http://mostlyerlang.com/2015/05/12/067-rebar-3/?get">>,
        <<"http://mostlyerlang.com/2015/05/12/067-rebar-3/get?a=b?d=c">>,
        <<"http://mostlyerlang.com/">>,
        <<"http://mostlyerlang.com">>,
        <<"http.mostlyerlang.com">>,
        <<"http://">>
    ],
    lists:map(fun parse/1, URLs).


-spec parse(binary()) -> map().
parse(URL) ->
    {URL, element(2,
        pipeline(URL, #{},
            [
                fun get_protocol/2,
                fun get_domain/2,
                fun get_query/2,
                fun get_path/2,
                fun get_date/2
            ]))}.


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
        [<<>>] -> {error, invalid_domains};
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
                   Res = pipeline([Y, M, D], [], [fun get_int/2, fun get_int/2, fun get_int/2]),
                   case Res of
                       {[], [D2, M2, Y2]} -> {Y2, M2, D2};
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