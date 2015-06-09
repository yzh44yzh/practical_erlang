# Многопоточность

Эффективная поддержка многопоточности -- одна из главных фишек эрланг.
И она же является базой для других фишек: распределенности, устойчивости
к ошибкам и горячему обновлению кода.


## Легковесные потоки

Эрланг имеет собственную реализацию многопоточности на уровне виртуальной машины.
Конечно, это работает поверх процессов операционной системы. Но поверх одного
такого процесса могут работать сотни и тысячи потоков эрланг. И виртуальная машина
управляет ими независимо от операционной системы.

В разных операционных системах есть разные сущности: процессы, потоки, нити и т.д.
Они отличаются реализацией и возможностями. Но в эрланг такая сущность только одна.
Я буду называть ее "поток". Но если где-то упомяну "процесс", то знайте, что это
одно и то же :)

Особенность потоков эрланг в том, что они легковесные. Это значит, что они:
- быстро стартуют и завершаются;
- быстро переключаются;
- потребляют мало памяти.

Новый поток создается и стартует за 3-5 микросекунд. На старте он
получает 2,5Кб памяти (стек, куча и служебная информация о потоке).

Виртуальная машина имеет лимит на число потоков, по умолчанию это
262,144 (2^18).  Но лимит можно увеличить до 134,217,727 (2^27).
Но вряд ли кому-то захочется создать 134 млн потоков на одной ноде,
тем более, что им понадобится 336Гб оперативной памяти :)

Легковесность потоков качественно меняет архитектуру.
Потоки не являются ограниченным ресурсом, как в большинстве других языков.
Можно легко выделить отдельный поток на обслуживание каждого клиента,
и позволить клиенту занимать этот поток сколько угодно долго.

Поэтому типичная область применения эрланг -- это сервера, которые должны
обслуживать большое количество клиентов. Особенно если соединения с клиентами
являются долгоживущими.

Кроме этого, потоки эрланг:
- одинаковые во всех операционных системах;
- имеют каждый свою изолированную область памяти (стек и кучу);
- не читают и не пишут в чужую память, а обмениваются сообщениями.


## Работа с потоками на низком уровне

### spawn

Для создания нового потока используется функция [spawn](http://www.erlang.org/doc/man/erlang.html#spawn-1).

Она имеет несколько вариантов:
- spawn(Fun) -> pid()
- spawn(Node, Fun) -> pid()
- spawn(Module, Function, Args) -> pid()
- spawn(Node, Module, Function, Args) -> pid()

Так или иначе в аргументах spawn указывается точка входа для нового потока, с которой
он начинает выполнятся. Далее поток либо выполнит весь код, и завершится. Либо попадет
в бесконечную рекурсию и будет выполнятся бесконечно. Либо поток завершится аварийно
из-за ошибки.

Функция spawn возвращает **Pid** -- идентификатор процесса (process identifier).
Зная Pid, можно посылать процессу сообщения и получать информацию о нем.

Давайте попробуем запустить несколько потоков:

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

Создаем анонимную функцию, в которой после паузы в 10 милисекунд
выводим на консоль ее аргумент.  С помощью генератора списков создаем
10 потоков. Каждый из них выполняет эту функцию независимо от
остальных. Видим, что очередность вывода чисел на консоль не совпадает
с очередностью создания потоков. Вывод будет случайным, в зависимости
от того, как планировщик раздавал управление потокам.


### Отправка сообщений

TODO stopped here

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

```erlang
10> self() ! hello.
hello
11> flush().
Shell got hello
ok
```


### Почтовый ящик
receive, selective receive

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


```erlang
12> self() ! "hello again".
"hello again"
13> receive
13> Msg -> io:format("got message:~p~n", [Msg])
13> end.
got message:"hello again"
ok
```

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


### Планировщики
по числу ядер
умеет передавать друг другу процессы, балансируя нагрузку на ядра

 the VM starts one thread per core which acts as a scheduler.
 Each of these schedulers has a run queue, or a list of Erlang processes on which to spend a slice of time.


### Выводы

Легкие потоки, обмен сообщениями, отсутсвие разделяемой памяти дают хорошую базу для:

  - масштрабируемости
  - распределенности
  - устойчивости к ошибкам

Масштабируемость -- бла бла бла.

Распределенность -- если мы научились взаимодествовать между 2 процессами, то потом не важно, в одной они ноде, или на разных.

Обработка ошибок -- бла бла бла, об этом отдельный урок.
