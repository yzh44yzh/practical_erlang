-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    Parts = binary:split(Str, [<<"{{">>], [global]),
    Parts2 =
        lists:map(fun(Part) ->
                          case binary:split(Part, [<<"}}">>]) of
                              [PartWithNoParam] -> PartWithNoParam;
                              [Param | Rest] ->
                                  case maps:find(Param, Data) of
                                      error -> Rest;
                                      {ok, Value} when is_binary(Value) orelse is_list(Value) ->
                                          [Value, Rest];
                                      {ok, Value} when is_integer(Value) ->
                                          [integer_to_binary(Value), Rest]
                                  end
                          end
                  end, Parts),
    unicode:characters_to_binary(Parts2).
