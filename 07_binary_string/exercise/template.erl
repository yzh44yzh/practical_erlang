-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    Str.
