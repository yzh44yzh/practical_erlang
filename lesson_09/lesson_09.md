# gen_server


Жизнь erlang-процесса
бесконечная рекурсия, состояние на стеке


Armstrong
12.2 Introducing Client-Server

И у Фреда аналогичное описано. И у других, наверное.
http://learnyousomeerlang.com/more-on-multiprocessing

тож самое у меня в erlang-school

В Ульяновсе я еще показывал, как работает hot code upgrade,
и использовал его, развивая gen\_server. Т.е. запускал новые версии gen\_server без остановки ноды
надо и в уроке так сделать. В erlang-school этого момента нет.


важность хвостовой рекурсии


## Жизнь erlang-процесса

бесконечная рекурсия, состояние на стеке

```erlang
-module(lifecycle).
-export([start/0, loop/1]).
start() ->
    InitialState = [],
    spawn(?MODULE, loop, [InitialState]).
loop(State) ->
    receive
        {add, Item} -> io:format("~p adds ~p to its state~n", [self(), Item]),
                       NewState = [Item | State],
                       ?MODULE:loop(NewState);
        {remove, Item} -> NewState = case lists:member(Item, State) of
                                         true -> lists:delete(Item, State);
                                         false -> io:format("I have no ~p~n", [Item]),
                                                  State
                                     end,
                          ?MODULE:loop(NewState);
        show_items -> io:format("my items is ~p~n", [State]),
                      ?MODULE:loop(State);
        stop -> io:format("~p stops now ~n", [self()]);
        _Any -> ?MODULE:loop(State)
    end.
```
