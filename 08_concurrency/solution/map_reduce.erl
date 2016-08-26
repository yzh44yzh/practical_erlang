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


%% Reduce process

reduce(Num, From) ->
    DataParts = [wait_data() || _ <- lists:seq(1, Num)],
    Data = lists:foldl(fun add_part/2, #{}, DataParts),
    From ! {ok, Data},
    ok.

wait_data() ->
    receive
        {ok, Data} -> Data;
        invalid_file -> #{}
    after
        2000 -> #{}
    end.

add_part(DataPart, Acc0) ->
    maps:fold(
      fun(Word, Num, Acc) ->
              case maps:find(Word, Acc) of
                  {ok, OldNum} -> Acc#{Word => Num + OldNum};
                  error -> Acc#{Word => Num}
              end
      end, Acc0, DataPart).


%% Map process

map(File, Reduce) ->
    Data = case file:read_file(File) of
               {ok, Bin} -> {ok, parse(Bin)};
               {error, _} -> invalid_file
           end,
    Reduce ! Data,
    ok.

parse(Bin) ->
    Words = binary:split(Bin, [<<" ">>, <<"\n">>, <<"\r">>], [global, trim]),
    lists:foldl(
      fun(Word, Acc) ->
              case maps:find(Word, Acc) of
                  {ok, Num} -> Acc#{Word => Num + 1};
                  error -> Acc#{Word => 1}
              end
      end, #{}, Words).
