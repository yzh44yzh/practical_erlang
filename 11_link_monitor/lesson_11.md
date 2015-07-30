# Обработка ошибок на низком уровне

Одна из главных фич эрланг -- устойчивость к ошибкам (fault
tolerance).  Считается, что система, сделанная на эрланг, может легко
переживать ошибки в коде и в данных, аппаратные сбои, сбои в сети, и
продолжать обслуживать клиентов.

Это не значит, что любой код на эрланг сразу обладает такими
свойствами.  Об этом должен позаботиться программист. А эрланг только
дает средства, которыми программист может обеспечить устойчивость.
Давайте посмотрим, что это за средства.


## link

Устойчивость к ошибкам построена на способности потоков наблюдать друг
за другом.  Два потока можно связать друг с другом так, что при
падение одного, второй получит специальное сообщение и тоже упадет.

Можно связать группу потоков, так, что при падении одного из них,
упадет вся группа. Предполагается, что потоки зависят друг от друга в
своей работе. Отсутствие одного потока приводит к нештатной ситуации,
в которой остальные не могут выполнять осмысленных действий. Они
только усугубляют и распространяют проблемы. Так что лучше остановить
и рестартовать всю группу.

Информация о падении передается в системном сообщении, тем же
способом, что и обычные сообщения между потоками. Но системные сообщения
(их еще называют **сигналы**), не попадают в почтовый ящик. Их нельзя
обработать обычным способом. Вместо этого, они просто завершают поток,
который их получил, и распространяются дальше по имеющимся связям.

При нормальной остановке потока сигнал распространяется, но не
вызывает завершение связанных потоков.

Вызов **link(Pid)** создает связь между текущим потоком и Pid.
Связь двухстороннаяя. Чтобы связать несколько потоков, нужно сделать
несколько вызовов link. Если потоки уже связаны, то вызов link не
оказывает никакого эффекта.

Вызов **unlink(Pid)** разрывает связь.

Часто бывает нужно создать новый поток и связь с ним. Это можно сделать
последовательными вызовами spawn и link. Но поток может завершиться
до вызова link. Поэтому лучше использовать функцию **spawn_link**,
которая объединяет эти две операции, но выполняется атомарно.


Рассмотрим пример кода:
```erlang
-module(sample1).

-export([run/0, run_and_crash/0, work/1, work_and_crash_one/1]).

run() ->
    [spawn_link(?MODULE, work, [Id]) || Id <- lists:seq(0, 5)],
    ok.

work(Id) ->
    io:format("~p ~p started~n", [Id, self()]),
    timer:sleep(1000),
    io:format("~p ~p stopped~n", [Id, self()]),
    ok.
```

Здесь мы запускаем 5 потоков, и связываем их все с потоком консоли.
Они стартуют, ждут 1 секунду, и завершаются:

```erlang
2> sample1:run().
0 <0.40.0> started
1 <0.41.0> started
2 <0.42.0> started
3 <0.43.0> started
4 <0.44.0> started
5 <0.45.0> started
ok
5 <0.45.0> stopped
4 <0.44.0> stopped
3 <0.43.0> stopped
2 <0.42.0> stopped
1 <0.41.0> stopped
0 <0.40.0> stopped
```

Теперь сделаем, чтобы один из потоков завершался. Это будет 3й поток,
как раз в середине всей группы потоков:

```erlang
run_and_crash() ->
    [spawn_link(?MODULE, work_and_crash_one, [Id]) || Id <- lists:seq(0, 5)],
    ok.

work_and_crash_one(Id) ->
    io:format("~p ~p started~n", [Id, self()]),
    if
        Id == 3 ->
            io:format("~p ~p exiting~n", [Id, self()]),
            exit(for_some_reason);
        true -> ok
    end,
    timer:sleep(1000),
    io:format("~p ~p stopped~n", [Id, self()]),
    ok.
```

Видим, что стартуют все потоки, но ни один из них не выполняется до конца,
потому что вместе с 3-м потоком завершились и все остальные.

```erlang
7> self().
<0.68.0>
8> sample1:run_and_crash().
0 <0.71.0> started
1 <0.72.0> started
2 <0.73.0> started
3 <0.74.0> started
4 <0.75.0> started
5 <0.76.0> started
3 <0.74.0> exiting
ok
** exception exit: for_some_reason
9> self().
<0.77.0>
```

В том числе и поток консоли, который завершился, и рестартовал заново.


## Системные потоки

Связи -- это хорошо, но этого мало. Наш следующий инструмент --
разделение ролей между потоками.  Есть рабочие потоки, которые делают
полезную работу.  И есть системные потоки, которые следят за
состоянием рабочих потоков.

Чтобы сделать поток системным, достаточно вызывать:
```erlang
process_flag(trap_exit, true)
```

При этом для потока устанавливается специальный флаг **trap_exit**.
После чего сигналы, которые они получает, превращаются в обычные
сообщения и попадают в почтовый ящик.  Таким образом системный поток
может обработать падение рабочего потока.

Сигнал превращается в сообщение вида:
```erlang
{'EXIT', Pid, Reason}
```

Это кортеж из 3х элеметов. На первой позиции атом 'EXIT', на второй --
Pid рабочего потока, на третей поциии -- причина завершения потока
(обычно атом или исключение).


TODO: пример кода
запустить и связать два потока, второй поток сделать системным,
завершить первый поток с ошибкой, получить сообщение во втором потоке,
показать это сообщение.


## Варианты событий при завершении потока

Поток может завершится двумя способами: нормально или аварийно.
В первом случае поток просто выполнил до конца ту функцию, с которой
он стартовал. Во втором случае произошла какая-то ошибка или исключение.

Кроме этого, поток можно принудительно завершить вызовом функции **exit**.
**exit/1** завершает текущий поток с заданной причиной:
```erlang
exit(normal)
exit(some_reason)
```

**exit/2** завершает поток с заданный Pid:
```erlang
exit(Pid, normal)
exit(Pid, some_reason)
```

Таким образом можно эмулировать и нормальное и аварийное завершение потока.

TODO: картинка с исходной моделью
W1   W4
|    |
W2 - S3 - S5

TODO: картинки с вариантами завершения работы

TODO: W2 завершается нормально
W1 игнорирует сигнал, S3 получает сообщение с reason=normal

TODO: W2 завершается аварийно
W1 завершается с тем же reason, S3 получает сообщение c reason=SomeError

TODO Тут рассказываем про Reason kill
и в каких ситуациях он нужен

While you can trap most exit reasons, there are situations where you
might want to brutally murder a process: maybe one of them is trapping
exits but is also stuck in an infinite loop, never reading any
message. The kill reason acts as a special signal that can't be
trapped. This ensures any process you terminate with it will really be
dead. Usually, kill is a bit of a last resort, when everything else
has failed.

When a system process receives a kill signal, it terminates. Kill signals
are generated by calling exit(Pid, kill) . This signal bypasses the normal error
signal processing mechanism and is not converted into a message. The
exit kill signal should be reserved for rogue processes that refuse to die
using any of the other error handling mechanisms.

TODO: W2 завершается с причиной kill
W1 завершается, S3 завершается, W4 завершается, S5 получает сообщение с reason=killed

TODO: разница между kill и killed и почему так

As the kill reason can never be trapped, it needs to be changed to
killed when other processes receive the message. If it weren't changed
in that manner, every other process linked to it would in turn die for
the same kill reason and would in turn kill its neighbors, and so
on. A death cascade would ensue.


## monitor

TODO

More seriously, monitors are a special type of link with two differences:
- they are unidirectional;
- they can be stacked.

Monitors are what you want when a process wants to know what's going
on with a second process, but neither of them really are vital to each
other.

Reference = erlang:monitor(process, Pid)

{'DOWN', Reference, process, Pid, Reason}

erlang:demonitor(Ref, [flush, info]).

The info option tells you if a monitor existed or not when you tried to remove it.
demonitor returned false.
Using flush as an option will remove the DOWN message from the mailbox if it existed

monitoring process
does not have to become a system process in order to handle errors.

Repeated calls to erlang:monitor(process,Pid) will return different ref-
erences, creating multiple independent monitors. They will all send their 'DOWN' mes-
sage when Pid terminates.


## Заключение

Использовать низкоуровневые функции **link/unlink** и создавать свои
системные процессы не рекомендуется. Нужен хороший опыт, чтобы
грамотно пользоваться этими средствами.  К счастью, это редко бывает
нужно, потому что у нас есть высокоуровневое средство -- супервизор.

Супервизор построен поверх **link** и **trap_exit**, построен хорошо
и отлажен годами использования в нагруженных проектах.  Вот его и
нужно использовать. Это тема следующего урока.

**monitor** используется чаще, когда нам не хватает тех вариантов
обработки, которые предлагает супервизор.
