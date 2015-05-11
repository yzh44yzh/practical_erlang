# Многопоточность в Erlang

## Concurrency and parallelism

Concurrency refers to the idea of having many actors running
independently, but not necessarily all at the same time.

Parallelism is having actors running exactly at the same time.

До R13B Эрланг работал на одном ядре процессора, и имел concurrency,
но не имел parallelism.

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

Пример кода:
генерируем много процессов, даем каждому задание
каждый выдает ответ после рандомной задержки,
наблюдаем, как ответы приходят в хаотичном порядке.

observer:start() -- показать потоки


На многопоточности основано:
- масштабируемость
- распределенность (если мы научились взаимодествовать между 2 процессами, то потом не важно, в одной они ноде, или на разных)
- обработка ошибок

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

### send message

Pid ! Message

Sends Message to the process with identifier Pid . Message sending is asyn-
chronous. The sender does not wait but continues with what it was doing.
! is called the send operator.

Pid ! M returns M . Because of this, Pid1 ! Pid2 ! ... ! Msg means send the
message Msg to all the processes Pid1 , Pid2 , and so on.

### receive

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


### register


## Почтовый ящик

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
- есть сообщения, но они не матчатся
- есть сообщения, и они матчатся
  (выбирается только одно из них)

selective receive or match all

timeout

с картинками и примерами кода


## Планировщики
