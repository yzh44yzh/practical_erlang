-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    State = your_state_structure,
    State.


create_short(LongLink, State) ->
    your_result.

get_long(ShortLink, State) ->
    {error, not_found}.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]).
