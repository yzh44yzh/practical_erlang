-module(main).

-export([parse/1, parse_file/2]).


parse(Files) ->
    Workers = lists:foldl(
                fun(File, Acc) ->
                        Pid = spawn(?MODULE, parse_file, [File, self()]),
                        Ref = erlang:monitor(process, Pid),
                        Acc#{ {Pid, Ref} => File }
                end,
                #{},
                Files),
    wait_data(Workers, {#{}, #{}}).


wait_data(Workers, Acc) when map_size(Workers) == 0 -> Acc;
wait_data(Workers, {DataAcc, ErrAcc}) ->
    receive
        {ok, Data} ->
            DataAcc2 = aggregate(Data, DataAcc),
            wait_data(Workers, {DataAcc2, ErrAcc});
        {'DOWN', Ref, process, Pid, Reason} ->
            Workers2 = maps:remove({Pid, Ref}, Workers),
            case Reason of
                normal -> wait_data(Workers2, {DataAcc, ErrAcc});
                _ ->
                    File = maps:get({Pid, Ref}, Workers),
                    ErrAcc2 = ErrAcc#{File => Reason},
                    wait_data(Workers2, {DataAcc, ErrAcc2})
            end
    after
        1000 -> {error, no_reply}
    end.


parse_file(File, ReducerPid) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    Data = lists:foldl(
             fun
                 (<<>>, Acc) -> Acc;
                 (Line, Acc) ->
                     [_Id, Name, Quantity, _Price]
                         = binary:split(Line, <<",">>, [global]),
                     Quantity2 = list_to_integer(binary_to_list(Quantity)),
                     Acc#{Name => Quantity2}
                      end,
             #{},
             Lines),
    ReducerPid ! {ok, Data},
    ok.


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
