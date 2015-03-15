-module(main).

-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, select/0, select/1]).

init() ->
    ets:new(my_ets, [named_table]),
    ets:insert(my_ets, [{1, "Bob", 25, male},
                        {2, "Helen", 17, female},
                        {3, "Bill", 28, male},
                        {4, "Kate", 22, female},
                        {5, "Ivan", 14, male}]),
    ok.


select() ->
    MS = ets:fun2ms(fun({Id, Name, Age, Gender})
                          when Age >= 17 andalso Gender =:= male ->
                            [Id, Name]
                    end),
    select(MS).


select(MS) ->
    ets:select(my_ets, MS).
