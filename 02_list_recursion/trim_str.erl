-module(trim_str).

-export([test_data/0, simple_trim/1, trim/1]).

-record(state, {
          is_leading_space = true :: boolean(),
          str_without_end_spaces = [] :: list(),
          str_with_spaces = [] :: list()
}).

test_data() ->
    "    bla bla    bla bla    ".


simple_trim(Str0) ->
    IsSpace = fun(Chr) -> Chr == 32 end,
    Str1 = lists:reverse(lists:dropwhile(IsSpace, Str0)),
    lists:reverse(lists:dropwhile(IsSpace, Str1)).


trim(Str0) ->
    trim(Str0, #state{}).

trim([], #state{str_without_end_spaces = Res}) -> lists:reverse(Res);

trim([32 | Rest], #state{is_leading_space = true} = State) ->
    trim(Rest, State);

trim([Chr | Rest], #state{is_leading_space = true, str_with_spaces = SS} = State) ->
    Str = [Chr | SS],
    trim(Rest, State#state{
                 is_leading_space = false,
                 str_without_end_spaces = Str,
                 str_with_spaces = Str
                 });

trim([32 | Rest], #state{is_leading_space = false, str_with_spaces = SS} = State) ->
    State2 = State#state{str_with_spaces = [32 | SS]},
    trim(Rest, State2);

trim([Chr | Rest], #state{is_leading_space = false, str_with_spaces = SS} = State) ->
    Str = [Chr | SS],
    State2 = State#state{str_without_end_spaces = Str, str_with_spaces = Str},
    trim(Rest, State2).
