-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    {dict:new(), dict:new()}.


create_short(LongLink, State) ->
    {LongDict, ShortDict} = State,
    case dict:find(LongLink, LongDict) of
        {ok, ShortLink} -> {ShortLink, State};
        error -> ShortLink = "http://hexlet.io/" ++ rand_str(8),
                 LongDict2 = dict:store(LongLink, ShortLink, LongDict),
                 ShortDict2 = dict:store(ShortLink, LongLink, ShortDict),
                 {ShortLink, {LongDict2, ShortDict2}}
    end.


get_long(ShortLink, State) ->
    {_, ShortDict} = State,
    case dict:find(ShortLink, ShortDict) of
        {ok, LongLink} -> {ok, LongLink};
        error -> {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]).
