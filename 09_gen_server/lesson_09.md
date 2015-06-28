# gen_server

Хороший подход к изучению gen_server – написать его самому. Такой
подход выбрали и Joe Armstrong (Programming Erlang, глава 16), и Fred
Hebert (LYSE, глава What is OTP?).

Пойдем и мы тем же путем. Напишем свой gen_server в 6 этапов.


## 1-й этап, простейший цикл.

Нам нужен поток, который никогда не завершается. Используем для этого
бесконечную рекурсию.

```
    -module(gs1).

    -export([start/0]).

    start() ->
        io:format("start ~p~n", [self()]),
        spawn(fun loop/0).

    loop() ->
        io:format("~p enters loop ~n", [self()]),
        receive
            stop -> io:format("~p stops now ~n", [self()]);
            Msg -> io:format("~p receive ~p~n", [self(), Msg]),
                   loop()
        end.
```

Поток запускается, входит в функцию **loop**, проверяет почтовый ящик,
обрабатывает сообщения, и опять входит в функцию **loop**.  Тут важно,
чтобы это была хвостовая рекурсия. Иначе будет расти память на стеке и
в какой-то момент нода упадет из-за нехватки памяти.

Можно предусмотреть нормальное завершение потока. Для этого добавим
обработку сообщения **stop**, получив которое, поток не будет вызывать
**loop**. И, таким образом, завершится.

```
1> c(gs1).
{ok,gs1}
2> Pid = gs1:start().
start <0.33.0>
<0.40.0> enters loop
<0.40.0>
3> Pid ! hello.
<0.40.0> receive hello
<0.40.0> enters loop
hello
4> Pid ! [1,2,3].
<0.40.0> receive [1,2,3]
<0.40.0> enters loop
[1,2,3]
5> Pid ! stop.
<0.40.0> stops now
stop
```

## 2-й этап, цикл c состоянием.

Добавим хранимое состояние.

```
    -module(gs2).

    -export([start/0, loop/1]).

    start() ->
        InitialState = [],
        spawn(?MODULE, loop, [InitialState]).

    loop(State) ->
        receive
            {add, Item} -> io:format("~p adds ~p to its state~n", [self(), Item]),
                           NewState = [Item | State],
                           loop(NewState);
            {remove, Item} -> NewState = case lists:member(Item, State) of
                                             true -> lists:delete(Item, State);
                                             false -> io:format("I have no ~p~n", [Item]),
                                                      State
                                         end,
                              loop(NewState);
            show_items -> io:format("my items is ~p~n", [State]),
                          loop(State);
            stop -> io:format("~p stops now ~n", [self()]);
            _Any -> loop(State)
        end.
```

Теперь функция **loop** получает аргумент. Это состояние потока. После
**spawn** он имеет некое начальное состояние. В данном случае это
массив. Но это может быть любая структура данных.

Затем поток может модифицировать эту структуру, и в последующие вызовы
**loop** передавать новое состояние. Таким образом, не имея
изменяемых переменных, мы все-таки имеем изменяемое состояние потока в
его стеке.

Тут мы усложнили форматы сообщений, которые умеет обрабатывать поток.
И сформировали некое АПИ: добавление и удаление элементов и вывод их
на консоль.

```
1> c(gs2).
{ok,gs2}
2> Pid = gs2:start().
<0.40.0>
3> Pid ! {add, 1}.
<0.40.0> adds 1 to its state
{add,1}
4> Pid ! {add, "hello"}.
<0.40.0> adds "hello" to its state
{add,"hello"}
5> Pid ! show_items.
my items is ["hello",1]
show_items
6> Pid ! {remove, "hello"}.
{remove,"hello"}
7> Pid ! show_items.
my items is [1]
show_items
```

## 3-й этап, горячее обновление кода.

```
    -module(gs3).

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

Здесь мы заменили вызовы **loop(State)** на **?MODULE:loop(State)**.
Тем самым мы заменили локальный вызов функции (только по ее имени),
на глобальный вызов (по имени модуля и функции). Для глобального
вызова действует горячее обновление кода. Как это работает?

Нода может держать в памяти 2 версии модуля. Допустим, при создании
потока и вызове loop, он начал выполнять версию 1 и прошел несколько
итераций рекурсии. Тем временем, мы изменили код, скомпилировали, и
загрузили в ноду версию 2. Пока текущая итерация не завершена, поток
все еще выполняет версию 1. Но следующий вызов loop уже попадет в
версию 2.

На следующих этапах мы уже не будем останавливать поток, а будем
пользоваться горячим обновлением кода.


## 4-й этап, синхронное АПИ.

Это хорошо, что наш сервер умеет хранить состояние и менять его в
зависимости от запросов клиентов. Но было бы неплохо, чтобы сервер
умел что-нибудь отвечать клиенту.

Для этого клиент должен передать свой Pid внутри сообщения.  А сервер
должен послать ответ на этот Pid.

Но мы не хотим делать взаимодействие с сервером слишком сложным для
клиента, поэтому спрячем отправку сообщений и получения ответов внутри
функций.

```
    -module(gs4).

    -export([start/0, add_item/2, remove_item/2, show_items/1, stop/1, loop/1]).


    start() ->
        InitialState = [],
        spawn(?MODULE, loop, [InitialState]).


    add_item(Pid, Item) ->
        Pid ! {add, self(), Item},
        receive
            {reply, Reply} -> Reply
        end.


    remove_item(Pid, Item) ->
        Pid ! {remove, self(), Item},
        receive
            {reply, Reply} -> Reply
        end.


    show_items(Pid) ->
        Pid ! {show_items, self()},
        receive
            {reply, Reply} -> Reply
        end.


    stop(Pid) ->
        Pid ! stop,
        ok.


    loop(State) ->
        receive
            {add, From, Item} ->
                NewState = [Item | State],
                From ! {reply, ok},
                ?MODULE:loop(NewState);
            {remove, From, Item} ->
                {Reply, NewState} = case lists:member(Item, State) of
                                        true -> {ok, lists:delete(Item, State)};
                                        false -> {{error, not_exist}, State}
                                    end,
                From ! {reply, Reply},
                ?MODULE:loop(NewState);
            {show_items, From} ->
                From ! {reply, State},
                ?MODULE:loop(State);
            stop -> ok;
            _Any -> ?MODULE:loop(State)
        end.
```

Теперь для клиента взаимодействие с сервером выглядит как простые
вызовы функций.

```
1> c(gs4).
{ok,gs4}
2> Pid = gs4:start().
<0.40.0>
3> gs4:add_item(Pid, 1).
ok
4> gs4:add_item(Pid, 2).
ok
5> gs4:show_items(Pid).
[2,1]
6> gs4:remove_item(Pid, 1).
ok
7> gs4:show_items(Pid).
[2]
8> gs4:stop(Pid).
ok
```

## 5-й этап, матчинг сообщений по Ref.

Такой вариант еще далек от совершенства. Поток-клиент любое сообщение
в своем почтовом ящике считает ответом сервера. А там могут быть совсем
другие сообщения.

Чтобы исправить эту проблему, нужно добавить уникальный идентификатор
в сообщение, и вернуть его в ответе. Для таких целей у нас есть
отдельный тип данных -- **reference**, и функция **make_ref**, которая
умеет генерировать уникальные значения такого типа.

```
    -module(gs5).

    -export([start/0, add_item/2, remove_item/2, show_items/1, stop/1, loop/1]).


    start() ->
        InitialState = [],
        spawn(?MODULE, loop, [InitialState]).


    add_item(Pid, Item) ->
        Ref = make_ref(),
        Pid ! {add, self(), Ref, Item},
        receive
            {reply, Ref, Reply} -> Reply
        end.


    remove_item(Pid, Item) ->
        Ref = make_ref(),
        Pid ! {remove, self(), Ref, Item},
        receive
            {reply, Ref, Reply} -> Reply
        end.


    show_items(Pid) ->
        Ref = make_ref(),
        Pid ! {show_items, self(), Ref},
        receive
            {reply, Ref, Reply} -> Reply
        end.


    stop(Pid) ->
        Pid ! stop,
        ok.


    loop(State) ->
        receive
            {add, From, Ref, Item} ->
                NewState = [Item | State],
                From ! {reply, Ref, ok},
                ?MODULE:loop(NewState);
            {remove, From, Ref, Item} ->
                {Reply, NewState} = case lists:member(Item, State) of
                                        true -> {ok, lists:delete(Item, State)};
                                        false -> {{error, not_exist}, State}
                                    end,
                From ! {reply, Ref, Reply},
                ?MODULE:loop(NewState);
            {show_items, From, Ref} ->
                From ! {reply, Ref, State},
                ?MODULE:loop(State);
            stop -> ok;
            _Any -> ?MODULE:loop(State)
        end.
```

Этот вариант работает также, как и предыдущий, но более надежен.

```
1> c(gs5).
{ok,gs5}
2> Pid = gs5:start().
<0.40.0>
3> gs5:add_item(Pid, "hello").
ok
4> gs5:add_item(Pid, "hi there").
ok
5> gs5:show_items(Pid).
["hi there","hello"]
6> gs5:remove_item(Pid, "hi there").
ok
7> gs5:show_items(Pid).
["hello"]
8> gs5:st
8> gs5:stop(Pid).
ok
```

## 6-й этап, убираем дублирование кода.

Ну вы уже заметили тут дублирование кода. И, наверняка, успели
возмутиться этим :) Что ж, пора от этого избавится. Обернем отправку
сообщения и получение ответа в отдельную функцию.

```
    -module(gs6).

    -export([start/0, add_item/2, remove_item/2, show_items/1, stop/1, loop/1]).


    start() ->
        InitialState = [],
        spawn(?MODULE, loop, [InitialState]).


    add_item(Pid, Item) ->
        call(Pid, {add, Item}).


    remove_item(Pid, Item) ->
        call(Pid, {remove, Item}).


    show_items(Pid) ->
        call(Pid, show_items).


    call(Pid, Msg) ->
        Ref = make_ref(),
        Pid ! {Msg, self(), Ref},
        receive
            {reply, Ref, Reply} -> Reply
        end.


    stop(Pid) ->
        Pid ! stop,
        ok.


    loop(State) ->
    receive
        {{add, Item}, From, Ref} ->
            NewState = [Item | State],
            From ! {reply, Ref, ok},
            ?MODULE:loop(NewState);
        {{remove, Item}, From, Ref} ->
            {Reply, NewState} = case lists:member(Item, State) of
                                    true -> {ok, lists:delete(Item, State)};
                                    false -> {{error, not_exist}, State}
                                end,
            From ! {reply, Ref, Reply},
            ?MODULE:loop(NewState);
        {show_items, From, Ref} ->
            From ! {reply, Ref, State},
            ?MODULE:loop(State);
        stop -> ok;
        _Any -> ?MODULE:loop(State)
    end.
```

Пришлось немного поменять формат сообщения и поправить шаблоны в **loop**.
Зато теперь отправка запроса и получение ответа у нас реализованы только в одном
месте, в функции **call**. И нам будет легко развивать эту логику дальше.


## 7-й этап, добавляем таймаут.

Осталось решить еще пару проблем, и у нас будет неплохое решение :)

Мы блокируем поток клиента, и это не всегда хорошо. Правильно будет позволить
клиенту задать timeout, как долго он готов ожидать ответ сервера.

timeout можно вынести в аргументы, и предоставить каждую АПИ-функцию в
двух вариантах, с явным указанием timeout и без указания. В настоящем
gen_server так и сделано.  Но мы сейчас не будем сильно усложнять код,
а просто добавим timeout в функцию call.

```
    call(Pid, Msg) ->
        Ref = make_ref(),
        Pid ! {Msg, self(), Ref},
        receive
            {reply, Ref, Reply} -> Reply
        after 5000 -> no_reply
        end.
```

## 8-й этап, монитор, обработка ошибок.

И последнее: если на сервере при обработке сообщения возникнет ошибка,
но неплохо было бы сообщить клиенту об этом.

Тут мы немного забегаем вперед, ибо обработка ошибок в одном потоке из
другого потока, это тема следующих уроков. Но хорошая реализация
требует, так что делаем :)

```
call(Pid, Msg) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {Msg, self(), MRef},
    receive
        {reply, MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            {error, Reason}
    after 5000 ->
            erlang:demonitor(MRef, [flush]),
            no_reply
    end.
```

Мы устанавливаем монитор, и теперь, если серверный поток упадет, то
клиентский получит сообщение **{'DOWN', MRef, process, Pid, Reason}**.
Когда клиент получает ответ, он снимает монитор. Так что
монитор действует только на время обработки запроса клиента.

И нам теперь не нужно создавать ссылку с помощью make_ref, потому что
monitor возвращает аналогичную ссылку.

## Итог

Итак, мы написали свой gen_server. Мы умеем:
- принимать запросы от клиента и отвечать на них;
- хранить и модифицировать состояние;
- делать горячее обновление кода;
- обрабатывать ошибки.

Настоящий gen\_server, входящий в состав OTP, устроен сложнее, конечно.
Но концептуально он работает именно так.

Код gen_server отшлифован за многие годы использования во многих
высоконагруженных проектах.  Так что нужно пользоваться именно им. И в
следующем уроке мы научимся этому.
