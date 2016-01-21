-module(main).

-export([parse/1]).


parse(Files) ->
    Workers = lists:map(
                fun(File) ->
                        Pid = spawn(worker, parse, [File, self()]),
                        Ref = erlang:monitor(process, Pid),
                        {Ref, File}
                end, Files),
    wait_results(Workers, {#{}, #{}}).


wait_results([], {Data, Errors}) -> {Data, Errors};
wait_results(Workers, {Data, Errors}) ->
    receive
        {ok, Result} ->
            Data2 = add_result(Result, Data),
            wait_results(Workers, {Data2, Errors});
        {'DOWN', Ref, process, _Pid, normal} ->
            Workers2 = lists:keydelete(Ref, 1, Workers),
            wait_results(Workers2, {Data, Errors});
        {'DOWN', Ref, process, _Pid, Error} ->
            {Ref, File} = lists:keyfind(Ref, 1, Workers),
            Workers2 = lists:delete({Ref, File}, Workers),
            Errors2 = maps:put(File, Error, Errors),
            wait_results(Workers2, {Data, Errors2})
    after
        2000 -> no_reply
    end.


add_result(Items, Data) ->
    lists:foldl(
      fun({Item, Quantity}, Acc) ->
              case maps:find(Item, Acc) of
                  {ok, Val} -> maps:put(Item, Quantity + Val, Acc);
                  error -> maps:put(Item, Quantity, Acc)
              end
      end, Data, Items).
