-module(main).

%% TODO:
%% - monitor
%% - crash


-export([test/0, start/1]).


test() ->
    start(["data_1.csv", "data_2.csv"]).


start(Files) ->
    Pids = [spawn(worker, parse, [File, self()]) || File <- Files],
    wait_results(length(Pids), []).


wait_results(0, Acc) -> reduce(Acc);
wait_results(Num, Acc) ->
    receive
        {ok, Result} -> wait_results(Num - 1, [Result | Acc])
    after
        2000 -> no_reply
    end.


reduce(ListsOfItems) ->
    lists:foldl(
      fun(List, Acc) ->
              lists:foldl(
                fun({Item, Quantity}, Acc2) ->
                        case maps:find(Item, Acc2) of
                            {ok, Val} -> maps:put(Item, Quantity + Val, Acc2);
                            error -> maps:put(Item, Quantity, Acc2)
                        end
                end, Acc, List)
      end, maps:new(), ListsOfItems).
