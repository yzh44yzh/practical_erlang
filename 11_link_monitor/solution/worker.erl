-module(worker).

-export([parse/2]).


parse(File, Pid) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    Res = lists:filtermap(fun process_line/1, Lines),
    Pid ! {ok, Res},
    ok.


process_line(<<>>) -> false;
process_line(Line) ->
    Parts = binary:split(Line, <<",">>, [global]),
    [_, Item, Quantity | _] = Parts,
    {true, {Item, list_to_integer(binary_to_list(Quantity))}}.
