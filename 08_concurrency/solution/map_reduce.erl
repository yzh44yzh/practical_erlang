-module(map_reduce).

-export([start/1]).
-export([reduce/2, map/2]).


start(Files) ->
    Num = length(Files),
    Reduce = spawn(?MODULE, reduce, [Num, self()]),
    [spawn(?MODULE, map, [File, Reduce]) || File <- Files],
    receive
        {ok, Data} -> Data
    after
        2000 -> timeout
    end.


reduce(Num, From) ->
    Data = reduce_loop(Num, maps:new()),
    From ! {ok, Data},
    ok.

reduce_loop(0, Acc) -> Acc;
reduce_loop(Num, Acc) ->
    receive
        {ok, Data} -> reduce_loop(Num - 1, merge(Data, Acc));
        invalid_file -> reduce_loop(Num - 1, Acc)
    end.

merge(Data, Acc0) ->
    lists:foldl(fun({Word, Num}, Acc) ->
                        case maps:find(Word, Acc) of
                            {ok, OldNum} -> maps:put(Word, Num + OldNum, Acc);
                            error -> maps:put(Word, Num, Acc)
                        end
                end,
                Acc0, maps:to_list(Data)).


map(File, Reduce) ->
    Data = case file:read_file(File) of
               {ok, Bin} -> {ok, parse(Bin)};
               {error, _} -> invalid_file
           end,
    Reduce ! Data,
    ok.

parse(Bin) ->
    Words = lists:map(fun unicode:characters_to_list/1,
                      binary:split(Bin, [<<" ">>, <<"\n">>, <<"\r">>], [global, trim])),
    lists:foldl(fun(Word, Map) ->
                        case maps:find(Word, Map) of
                            {ok, Num} -> maps:put(Word, Num + 1, Map);
                            error -> maps:put(Word, 1, Map)
                        end
                end, maps:new(), Words).
