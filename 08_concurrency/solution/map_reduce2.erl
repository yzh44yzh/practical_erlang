-module(map_reduce).

-export([
         start/1, analize_file/1, aggregate/1, aggregate/2,
         reducer/2, worker/2
]).


start(Files) ->
    RootPid = self(),
    NumWorkers = length(Files),
    ReducerPid = spawn(?MODULE, reducer, [RootPid, NumWorkers]),
    [spawn(?MODULE, worker, [ReducerPid, File]) || File <- Files],
    receive
        {ok, Result} -> Result
    after
        2000 -> {error, no_reply}
    end.


reducer(RootPid, NumWorkers) ->
    io:format("reducer started~n"),
    DataList = [wait_data() || _ <- lists:seq(1, NumWorkers)],
    Result = aggregate(DataList),
    RootPid ! {ok, Result},
    ok.

wait_data() ->
    receive
        {worker, Pid, File, Data} ->
            io:format("reducer got result from ~p, ~p~n", [Pid, File]),
            Data
    after
        1000 ->
            io:format("no data from workers~n"),
            #{}
    end.


worker(ReducerPid, File) ->
    io:format("worker for file ~p started~n", [File]),
    Data = analize_file(File),
    ReducerPid ! {worker, self(), File, Data},
    ok.


analize_file(File) ->
    case file:read_file(File) of
        {ok, Data} -> analize_data(Data);
        {error, _} -> #{}
    end.

analize_data(Data) ->
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
