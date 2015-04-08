# Многопоточность

## Concurrency and parallelism

Concurrency refers to the idea of having many actors running
independently, but not necessarily all at the same time.

Parallelism is having actors running exactly at the same time.

До R13B Эрланг работал на одном ядре процессора, и имел concurrency,
но не имел parallelism.

Корни эрланга в телефонии. Нужно обслуживать звонки. Много звонков, каждый независимо от других.
Отсюда легковестные потоки со своей памятью.


## Многопоточность в Erlang

Создание и остановка потока очень быстрые (3-5 микросекунд).
Потоки легкие, 2.5Кб памяти на старте.

```erlang
4> G = fun(X) -> timer:sleep(10), io:format("~p~n", [X]) end.
 #Fun<erl_eval.6.13229925>
5> [spawn(fun() -> G(X) end) || X <- lists:seq(1,10)].
[<0.273.0>,<0.274.0>,<0.275.0>,<0.276.0>,<0.277.0>,
<0.278.0>,<0.279.0>,<0.280.0>,<0.281.0>,<0.282.0>]
2
1
4
3
5
8
7
6
10
9
```

Несколько вариантов spawn
http://www.erlang.org/doc/man/erlang.html#spawn-1
spawn(Fun) -> pid()
spawn(Node, Fun) -> pid()
spawn(Module, Function, Args) -> pid()
spawn(Node, Module, Function, Args) -> pid()


Показать, что shell тож процесс, что у него есть pid, он рестартует при краше,
```erlang
1>
1> A = 128.
128
2> B = 512.
512
5> self().
<0.33.0>
6> exit(self()).
** exception exit: <0.33.0>
7> self().
<0.40.0>
8> A.
128
9> B.
512
```
биндинги при этом не теряются


### Отправка сообщений

```erlang
10> self() ! hello.
hello
11> flush().
Shell got hello
ok
```


### Почтовый ящик
receive, selective receive

```erlang
12> self() ! "hello again".
"hello again"
13> receive
13> Msg -> io:format("got message:~p~n", [Msg])
13> end.
got message:"hello again"
ok
```

### Планировщики
по числу ядер
умеет передавать друг другу процессы, балансируя нагрузку на ядра


### register


### Выводы

легкие потоки, обмен сообщениеями, отсутсвие разделяемой памяти дают хорошую базу для:

  - масштрабируемости
  - распределенности
  - устойчивости к ошибкам


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

важность хвостовой рекурсии

observer:start()


## Обработка ошибок на низком уровне

link, spawn_link
{'EXIT', Pid, Reason}

exit(Pid, Reason)

**System processes** are basically normal processes, except they can
convert exit signals to regular messages. This is done by calling
process_flag(trap_exit, true) in a running process.

сигнал 'EXIT' превращает в сообщение 'EXIT', которое можно обработать.

|--------------+--------------------------------+-------------------------------|
| Reason       | trap_exit=true                 | trap_exit=false               |
|--------------+--------------------------------+-------------------------------|
| exit(normal) | Receives {'EXIT', Pid, normal} | Nothing happens               |
|--------------+--------------------------------+-------------------------------|
| exit(kill)   | Terminates with reason killed  | Terminates with reason killed |
|--------------+--------------------------------+-------------------------------|
| exit(Other)  | Receives {'EXIT', Pid, Other}  | Terminates with reason Other  |
|--------------+--------------------------------+-------------------------------|

monitor
{'DOWN', Reference, process, Pid, Reason}
