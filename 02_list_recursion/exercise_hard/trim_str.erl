-module(trim_str).

-export([test_data/0, simple_trim/1, trim/1]).

test_data() ->
    "    bla bla    bla bla    ".


simple_trim(Str0) ->
    IsSpace = fun(Chr) -> Chr == 32 end,
    Str1 = lists:reverse(lists:dropwhile(IsSpace, Str0)),
    lists:reverse(lists:dropwhile(IsSpace, Str1)).


-record(state, {
          met_non_space = false,
          result = [],
          spaces = []
         }).

-define(SPACE, 32).

trim(Str) ->
    trim(Str, #state{}).

trim([], #state{result = Res}) -> lists:reverse(Res);

%% space at the beginning of Str
trim([?SPACE | Tail], #state{met_non_space = false} = State) ->
    trim(Tail, State);

%% switch met_non_space flag
trim([Char | Tail], #state{met_non_space = false, result = Res} = State) ->
    State1 = State#state{met_non_space = true, result = [Char | Res]},
    trim(Tail, State1);

trim([Char | Tail], #state{met_non_space = true, result = Res, spaces = Spaces} = State) ->
    State1 = if
                 Char == ?SPACE ->
                     State#state{spaces = [Char | Spaces]};
                 Char /= ?SPACE ->
                     State#state{result = [Char | Spaces ++ Res], spaces = []}
             end,
    trim(Tail, State1).
