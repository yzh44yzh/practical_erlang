-module(main).

-export([parse/1, parser/2]).

%% Root process

parse(Files) ->
    Workers = lists:foldl(
        fun(File, Acc) ->
            Pid = spawn(?MODULE, parser, [File, self()]),
            Ref = erlang:monitor(process, Pid),
            Acc#{{Pid, Ref} => File}
        end,
        #{},
        Files),
    wait_for_data(Workers, {#{}, #{}}).


wait_for_data(Workers, Acc) when map_size(Workers) == 0 -> Acc;
wait_for_data(Workers, {DataAcc, ErrAcc}) ->
    receive
        {result, Data} ->
            DataAcc2 = aggregate(Data, DataAcc),
            wait_for_data(Workers, {DataAcc2, ErrAcc});
        {'DOWN', Ref, process, Pid, Reason} ->
            Workers2 = maps:remove({Pid, Ref}, Workers),
            case Reason of
                normal -> wait_for_data(Workers2, {DataAcc, ErrAcc});
                _ ->
                    File = maps:get({Pid, Ref}, Workers),
                    ErrAcc2 = ErrAcc#{File => Reason},
                    wait_for_data(Workers2, {DataAcc, ErrAcc2})
            end
    after
        1000 -> {error, no_reply}
    end.


%% Parser process

parser(File, RootPid) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    Data = parse_lines(Lines),
    RootPid ! {result, Data},
    ok.


%% Utils

parse_lines(Lines) ->
    lists:foldl(
        fun
            (<<>>, Acc) -> Acc;
            (Line, Acc) ->
                [_Id, Name, Quantity, _Price]
                    = binary:split(Line, <<",">>, [global]),
                Quantity2 = list_to_integer(binary_to_list(Quantity)),
                Acc#{Name => Quantity2}
        end,
        #{},
        Lines).


aggregate(Data1, Data2) ->
    maps:fold(
        fun(Name, Quantity, Acc) ->
            case maps:find(Name, Data2) of
                {ok, Quantity2} -> Acc#{Name => Quantity + Quantity2};
                error -> Acc#{Name => Quantity}
            end
        end,
        Data2,
        Data1).
