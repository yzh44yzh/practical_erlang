-module(map_reduce).

-export([
    start/1, reducer/2, mapper/2,
    analyze_file/1, aggregate/1, aggregate/2
]).


start(Files) ->
    RootPid = self(),
    NumWorkers = length(Files),
    ReducerPid = spawn(?MODULE, reducer, [RootPid, NumWorkers]),
    [spawn(?MODULE, mapper, [File, ReducerPid]) || File <- Files],
    receive
        {result, Result} -> Result
    after
        2000 -> {error, no_reply}
    end.


%% Reduce process

reducer(RootPid, NumWorkers) ->
    io:format("reducer started~n"),
    DataList = [wait_for_data() || _ <- lists:seq(1, NumWorkers)],
    Result = aggregate(DataList),
    RootPid ! {result, Result},
    ok.


wait_for_data() ->
    receive
        {mapper, Pid, File, Data} ->
            io:format("reducer got result from ~p, ~p~n", [Pid, File]),
            Data
    after
        1000 ->
            io:format("no data from mapper~n"),
            #{}
    end.


%% Map process

mapper(File, ReducerPid) ->
    io:format("mapper for file ~p started~n", [File]),
    Data = analyze_file(File),
    ReducerPid ! {mapper, self(), File, Data},
    ok.


%% Utils

analyze_file(File) ->
    case file:read_file(File) of
        {ok, Data} -> analyze_data(Data);
        {error, _} -> #{}
    end.


analyze_data(Data) ->
    Words = binary:split(Data, [<<" ">>, <<"\n">>], [global]),
    lists:foldl(
      fun
          (<<>>, Acc) -> Acc;
          (Word, Acc) ->
              case maps:find(Word, Acc) of
                  {ok, Counter} -> Acc#{Word => Counter + 1};
                  error -> Acc#{Word => 1}
              end
      end,
      #{},
      Words).


aggregate(DataList) ->
    lists:foldl(
      fun(Data, Acc) -> aggregate(Data, Acc) end,
      #{},
      DataList).


aggregate(Data1, Data2) ->
    maps:fold(
      fun(Word, Counter, Acc) ->
              case maps:find(Word, Data2) of
                  {ok, Counter2} -> Acc#{Word => Counter + Counter2};
                  error -> Acc#{Word => Counter}
              end
      end,
      Data2,
      Data1).
