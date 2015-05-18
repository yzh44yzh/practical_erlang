# Многопоточность в Erlang

## Concurrency and parallelism

Concurrency refers to the idea of having many actors running
independently, but not necessarily all at the same time.

Parallelism is having actors running exactly at the same time.

До R13B Эрланг работал на одном ядре процессора, и имел concurrency,
но не имел parallelism. 2009 year

If we have only a single-core computer, then we can never run a parallel
program on it. This is because we have one CPU, and it can do only one thing
at a time. We can, however, run concurrent programs on a single-core com-
puter. The computer time-shares between the different tasks, maintaining
the illusion that the different tasks run in parallel.


## Легковесные потоки

In Erlang, processes belong to the programming language and not the
operating system.

- Creating and destroying processes is very fast.
- Sending messages between processes is very fast.
- Processes behave the same way on all operating systems.
- We can have very large numbers of processes.
- Processes share no memory and are completely independent.
- The only way for processes to interact is through message passing.

Создание и остановка потока очень быстрые (3-5 микросекунд).
Потоки легкие, 2.5Кб памяти на старте.

TODO: Armstrong 12.3 Processes Are Cheap
TODO: в моем блоге об этом

soft real time
TODO что это такое и как это отличается от hard real time?

This gives system response times on the order of
milliseconds even in the presence of garbage-collected memory.


Пример кода:
генерируем много процессов, даем каждому задание
каждый выдает ответ после рандомной задержки,
наблюдаем, как ответы приходят в хаотичном порядке.

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
The order doesn't make sense. Welcome to parallelism.

observer:start() -- показать потоки


На многопоточности основано:
- масштабируемость
- распределенность (если мы научились взаимодествовать между 2 процессами, то потом не важно, в одной они ноде, или на разных)
- обработка ошибок (об этом отдельный урок)

Принципиальное значение для архитектуры:
процессы не являются ограниченным ресурсом, как в большинстве других языков
создание, завершение, переключение не являются дорогими операциями

Отсюда легко работать с веб-сокетами, например, ибо легко можно выделить отдельный
процесс каждому соединению. Чего многие другие платформы не могут себе позволить.


## Работа с потоками на низком уровне

### spawn

http://www.erlang.org/doc/man/erlang.html#spawn-1

Pid = spawn(Mod, Func, Args)

много вариантов spawn, по-разному задающих функцию, + варианты для запуска на другой ноде

spawn returns a Pid (short for process identifier). You can use a Pid to send messages to the process.

If there is no more code to execute, a process is said to terminate normally. On the other
hand, if a runtime error such as a bad match or a case failure occurs, the process is said
to terminate abnormally.

Spawning a process will never fail, even if you are spawning a nonexported or even a
nonexistent function. As soon as the process is created and spawn/3 returns the pid, the
newly created process will terminate with a runtime error:

### send message

The only way for processes to interact with each other is through
message passing, where data is sent from one process to another.

Pid ! Message

Sends Message to the process with identifier Pid . Message sending is asyn-
chronous. The sender does not wait but continues with what it was doing.
! is called the send operator.

Pid ! M returns M . Because of this, Pid1 ! Pid2 ! ... ! Msg means send the
message Msg to all the processes Pid1 , Pid2 , and so on.

Sending a message will never fail; so if you try sending a message to a nonexistent
process, it is thrown away without generating an error

### receive

Each Erlang process has a mailbox in which incoming messages are stored. When a
message is sent, it is copied from the sending process into the recipient’s mailbox for
retrieval.

Messages are stored in the mailbox in the order in which they are delivered.

Receives a message that has been sent to a process. It has the following
syntax:
```erlang
receive
    Pattern1 [when Guard1] ->
        Expressions1;
    Pattern2 [when Guard2] ->
        Expressions2;
    ...
end
```

Аналогично case

As soon as the execution flow of the process reaches
the receive statement, it will try to match the oldest message in the mailbox
A receive statement will return the last evaluated expression in the body of the matched
clause.

process is suspended in receive
statements until a message is matched

нужен конкретный пример, прямо в консоли, как наполняется ящик сообщениями,
как они выбираются, и что остается в ящике
написать модуль для демонстрации этого
использовать process_info(messages), чтобы посмотреть, что в ящике


• Erlang programs are made of lots of processes. These processes can send
messages to each other.

• These messages may or may not be received and understood. If you want
to know whether a message was received and understood, you must send
the process a message and wait for a reply.

When you send a message to a process, the message is put into the mailbox
of the process. The only time the mailbox is examined is when your program
evaluates a receive statement.

если на момент receive:
- почтовый ящик пустой
  (блокируется, ждет сообщения)
- есть сообщения, но они не матчатся
  (блокируется, ждет сообщение, которое заматчится)
- есть сообщения, и они матчатся
  (выбирается только одно из них, остальные остаются в ящике до следующего вызова receive)

selective receive or match all

### timeout

receive
    ...
after
    Timeout -> exp1
end

Timeout is an integer denoting
the time in milliseconds, or the atom infinity .
Using infinity as a timeout value is the
same as not including the after construct.



### register

register(Alias, Pid)
unregister(Pid)
registered()  which returns a list of registered names
whereis(Alias)  which returns the pid associated with the Alias


## Планировщики

 the VM starts one thread per core which acts as a scheduler.
 Each of these schedulers has a run queue, or a list of Erlang processes on which to spend a slice of time.
