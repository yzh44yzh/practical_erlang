# Эрланг на практике. TCP и UDP сокеты.

Пора применить эрланг по его прямому назначению -- для реализации
сетевого сервиса.  Чаще всего такие сервисы делают на базе
веб-сервера, поверх протокола [HTTP](https://ru.wikipedia.org/wiki/HTTP).
Но мы возьмем уровень ниже -- TCP и UDP сокеты.

Я полагаю, вы уже знаете, как устроена сеть, что такое
[Internet Protocol](https://ru.wikipedia.org/wiki/IP),
[User Datagram Protocol](https://ru.wikipedia.org/wiki/UDP) и
[Transmission Control Protocol](https://ru.wikipedia.org/wiki/TCP).
Эта тема большинству программистов известна.  Но если вы почему-то ее
упустили, то придется сперва наверстать упущенное, и потом вернуться к
этому уроку.


## UDP сокет

Вспомним в общих чертах, что такое UDP:

 - протокол передачи коротких сообщений (Datagram);
 - быстрая доставка;
 - без постоянного соединения между клиентом и сервером, без состояния;
 - доставка сообщения и очередность доставки не гарантируется.

Для работы с UDP используется модуль [gen_udp](http://www.erlang.org/doc/man/gen_udp.html).

Давайте запустим две ноды и наладим общение между ними.

На 1-й ноде откроем UDP на порту 2000:

```
1> {ok, Socket} = gen_udp:open(2000, [binary, {active, true}]).
{ok,#Port<0.587>}
```

Вызываем **gen_udp:open/2**, передаем номер порта и список опций.
Список всех возможных опций довольно большой, но нас интересуют две из них:

_binary_ -- сокет открыт в бинарном режиме. Как вариант, сокет можно
открыть в текстовом режиме, указав опцию _list_. Разница в том, как мы
интерпретируем данные, полученные из сокета -- как поток байт, или как текст.

_{active, true}_ -- сокет открыт в активном режиме, значит данные,
приходящие в сокет, будут посылаться в виде сообщений в почтовый ящик потока,
владельца сокета. Подробнее об этом ниже.

На 2-й ноде откроем UDP на порту 2001:

```
1> {ok, Socket} = gen_udp:open(2001, [binary, {active, true}]).
{ok,#Port<0.587>}
```

И пошлем сообщение с 1-й ноды на 2-ю:

```
2> gen_udp:send(Socket, {127,0,0,1}, 2001, <<"Hello from 2000">>).
ok
```

Вызываем **gen_udp:send/4**, передаем сокет, адрес и порт получателя, и само сообщение.

Адрес может быть доменным именем в виде строки или атома, или адресом IPv4 в виде кортежа
из 4-х чисел, или адресом IPv6 в виде кортежа из 8 чисел.

На 2-й ноде убедимся, что сообщение пришло:

```
2> flush().
Shell got {udp,#Port<0.587>,{127,0,0,1},2000,<<"Hello from 2000">>}
ok
```

Сообщение приходит в виде кортежа _{udp, Socket, SenderAddress, SenderPort, Packet}_.

Пошлем сообщение с 2-й ноды на 1-ю:

```
3> gen_udp:send(Socket, {127,0,0,1}, 2000, <<"Hello from 2001">>).
ok
```

На 1-й ноде убедимся, что сообщение пришло:

```
3> flush().
Shell got {udp,#Port<0.587>,{127,0,0,1},2001,<<"Hello from 2001">>}
ok
```

Как видим, тут все просто.


## Активный и пассивный режим сокета

И **gen_udp**, и **gen_tcp**, оба имеют одну важную настройку: режим
работы с входящими данными.  Это может быть либо активный режим
_{active, true}_, либо пассивный режим _{active, false}_.

В активном режиме поток получает входящие пакеты в виде сообщений в
своем почтовом ящике.  И их можно получить и обработать вызовом
receive, как любые другие сообщения.

Для udp сокета это сообщения вида:

```
{udp, Socket, SenderAddress, SenderPort, Packet}
```

мы их уже видели:

```
{udp,#Port<0.587>,{127,0,0,1},2001,<<"Hello from 2001">>}
```

Для tcp сокета аналогичные сообщения:

```
{tcp, Socket, Packet}
```

Активный режим прост в использовании, но опасен тем, что клиент может
переполнить очередь сообщений потока, исчерпать память и обрушить
ноду.  Поэтому рекомендуется пассивный режим.

В пассивном режиме данные нужно забрать самому
вызовами **gen_udp:recv/3** и **gen_tcp:recv/3**:

```
gen_udp:recv(Socket, Length, Timeout) -> {ok, {Address, Port, Packet}} | {error, Reason}
gen_tcp:recv(Socket, Length, Timeout) -> {ok, Packet} | {error, Reason}
```

Здесь мы указываем, сколько байт данных хотим прочитать из
сокета. Если там есть эти данные, то мы получаем их сразу. Если нет,
то вызов блокируется, пока не придет достаточное количество данных. Можно
указать Timeout, чтобы не блокировать поток надолго.

Однако, **gen_udp:recv** игнорирует аргумент Length, и возвращает все
данные, которые есть в сокете. Или блокируется и ждет каких-нибудь
данных, если в сокете ничего нет. Непонятно, зачем вообще аргумент
Length присутствует в АПИ.

Для **gen_tcp:recv** аргумент Length работает как надо. Если только
не указана опция _{packet, Size}_, о которой речь пойдет ниже.

Еще есть вариант _{active, once}_. В этом случае сокет запускается в
активном режиме, получает первый пакет данных как сообщение, и сразу
переключается в пассивный режим.

И с 17-й версии эрланг добавился вариант _{active, Num}_, где
указывается количество пакетов, которые приходят в активном режиме,
после которого сокет переключается в пассивный режим.


## TCP сокет

Вспомним в общих чертах, что такое TCP:

 - надежный протокол передачи данных, гарантирует доставку сообщения и очередность доставки;
 - постоянное соединение клиента и сервера, имеет состояние;
 - дополнительные накладные расходы на установку и закрытие соединения и на передачу данных.

Надо заметить, что долго держать постоянные соединения с многими
тысячами клиентов накладно. Все соединения должны работать независимо
друг от друга, а это значит -- в разных потоках.  Для многих языков
программирования (но не для эрланг) это серьезная проблема.

Именно поэтому так популярен протокол HTTP, который хоть и работает
поверх TCP сокета, но подразумевает короткое время взаимодействия. Это
позволяет относительно небольшим числом потоков (десятки-сотни)
обслуживать значительно большее число клиентов (тысячи, десятки
тысяч).

В некоторых случаях остается необходимость иметь долгоживущие
постоянные соединения между клиентом и сервером. Например, для чатов
или для многопользовательских игр. И здесь эрланг имеет мало
конкурентов.

Для работы с TCP используется модуль [gen_tcp](http://www.erlang.org/doc/man/gen_tcp.html).

Работать с TCP сокетом сложнее, чем с UDP. У нас появляются роли клиента и сервера,
требующие разной реализации. Рассмотрим вариант реализации сервера.

```
-module(server).

-export([start/0, start/1, server/1, accept/2]).

start() ->
    start(1234).

start(Port) ->
    spawn(?MODULE, server, [Port]),
    ok.

server(Port) ->
    io:format("start server at port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    timer:sleep(infinity),
    ok.

accept(Id, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [Id]),
    {ok, _Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket #~p, session started~n", [Id]),
    handle_connection(Id, ListenSocket).

handle_connection(Id, ListenSocket) ->
    receive
        {tcp, Socket, Msg} ->
            io:format("Socket #~p got message: ~p~n", [Id, Msg]),
            gen_tcp:send(Socket, Msg),
            handle_connection(Id, ListenSocket);
        {tcp_closed, _Socket} ->
            io:format("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.
```

Есть два вида сокета: **Listen Socket** и **Accept Socket**. Listen
Socket один, он принимает все запросы на соединение.  Accept Socket
нужно много, по одному для каждого соединения. Поток, в котором
создается сокет, становится владельцем сокета.  Если поток-владелец
завершается, то сокет автоматически закрывается. Поэтому для каждого
сокета мы создаем отдельный поток.

Listen Socket должен работать всегда, а для этого его поток-владелец
не должен завершаться. Поэтому в **server/1** мы добавили вызов
_timer:sleep(infinity)_. Это заблокирует поток и не даст ему
завершиться.  Такая реализация, конечно, учебная. По хорошему нужно
предусмотреть возможность корректно остановить сервер, а здесь этого
нет.

Accept Socket и поток для него можно было бы создавать динамически, по
мере появления клиентов. В начале можно создать один такой поток,
вызвать в нем **gen_tcp:accept/1** и ждать клиента. Этот вызов
является блокирующим. Он завершается, когда появляется клиент.
Дальше можно обслуживать текущего клиента в этом потоке, и создать новый
поток, ожидающий нового клиента.

Но здесь у нас другая реализация. Мы заранее создаем пул из нескольких
потоков, и все они ждут клиентов. После завершения работы с одним клиентом
сокет не закрывается, а ждет нового. Таким образом, вместо того, чтобы
постоянно открывать новые сокеты и закрывать старые, мы используем пул
долгоживущих сокетов.

Это эффективнее при большом количестве клиентов. Во-первых, из-за того,
что мы быстрее принимаем соединения. Во-вторых, из-за того, что мы
более аккуратно распоряжаемся сокетами как системным ресурсом.

Потоки принадлежат эрланговской ноде, и мы можем создавать их сколько
угодно.  Но сокеты принадлежат операционной системе. Их количество
лимитировано, хотя и довольно большое. (Речь идет о лимите на
количество файловых дескрипторов, которое операционная система
позволяет открыть пользовательскому процессу, обычно это 2^10 - 2^16).

Размер пула у нас игрушечный -- 5 пар поток-сокет. Реально нужен пул
из нескольких сотен таких пар. Хорошо бы еще иметь возможность
увеличивать и уменьшать этот пул в рантайме, чтобы подстраиваться под
текущую нагрузку.

Текущая сессия с клиентом обрабатывается в функции **handle_connection/2**.
Видно, что сокет работает в активном режиме, и поток получает сообщения вида
_{tcp, Socket, Msg}_, где _Msg_ -- это бинарные данные, пришедшие от клиента.
Эти данные мы отравляет обратно клиенту, то есть, реализуем банальный эхо-сервис :)

Когда клиент закрывает соединение, поток получает сообщение
_{tcp_closed, _Socket}_, возвращается обратно в **accept/2** и ждет
следующего клиента.

Вот как выглядит работа такого сервера с двумя telnet-клиентами:

Клиент 1:

```
$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello from client 1
hello from client 1
some message from client 1
some message from client 1
new message from client 1
new message from client 1
client 1 is going to close connection
client 1 is going to close connection
^]
telnet> quit
Connection closed.
```

Клиент 2:

```
$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello from client 2
hello from client 2
message from client 2
message from client 2
client 2 is still active
client 2 is still active
but client 2 is still active
but client 2 is still active
and now client 2 is going to close connection
and now client 2 is going to close connection
^]
telnet> quit
Connection closed.
```

Сервер:

```
2> server:start().
start server at port 1234
ok
Socket #1 wait for client
Socket #2 wait for client
Socket #3 wait for client
Socket #4 wait for client
Socket #5 wait for client
Socket #1, session started
Socket #1 got message: <<"hello from client 1\r\n">>
Socket #1 got message: <<"some message from client 1\r\n">>
Socket #2, session started
Socket #2 got message: <<"hello from client 2\r\n">>
Socket #2 got message: <<"message from client 2\r\n">>
Socket #1 got message: <<"new message from client 1\r\n">>
Socket #2 got message: <<"client 2 is still active\r\n">>
Socket #1 got message: <<"client 1 is going to close connection\r\n">>
Socket #1, session closed
Socket #1 wait for client
Socket #2 got message: <<"but client 2 is still active\r\n">>
Socket #2 got message: <<"and now client 2 is going to close connection\r\n">>
Socket #2, session closed
Socket #2 wait for client
```


### Сервер в пассивном режиме

Это все хорошо, но хороший сервер должен работать в пассивном режиме.
То есть, он должен получать данные от клиента не в виде сообщений в
почтовый ящик, а вызовом **gen_tcp:recv/2,3**.

Нюанс в том, что тут нужно указать, сколько данных мы хотим прочитать.
А откуда сервер может знать, сколько данных ему прислал клиент?
Ну, видимо, клиент сам должен сказать, сколько данных он собирается
прислать.  Для этого клиент сперва посылает небольшой служебный
пакет, в котором указывает размер своих данных, и затем посылает сами
данные.

Например, если клиент хочет послать данные _<<"Hello">>_, размер
которых 5 байт, то он посылает сперва _<<5>>_, затем _<<"Hello">>_.
Соответственно, сервер сперва читает этот служебный пакет, и по нему
определяет, сколько данных нужно прочитать дальше.

Теперь нужно решить, сколько байт должен занимать этот служебный
пакет.  Если это будет 1 байт, то в него нельзя упаковать число
больше 255.  В 2 байта можно упаковать число 65535, в 4
байта 4294967295.  1 байт, очевидно, мало. Вполне вероятно, что
клиенту будет нужно послать данных больше, чем 255 байт. Заголовок в 2
байта вполне подходит. Заголовок в 4 байта иногда бывает нужен.

Итак, клиент посылает служебный пакет размером в 2 байта, где
указано, сколько данных последуют за ним, а затем сами эти данные:

```
Msg = <<"Hello">>,
Size = byte_size(Msg),
Header = <<Size:16/integer>>,
gen_tcp:send(Socket, <<Header/binary, Msg/binary>>),
```

Полный код клиента:

```
-module(client2).

-export([start/0, start/2, send/2, stop/1, client/2]).

start() ->
    start("localhost", 1234).

start(Host, Port) ->
    spawn(?MODULE, client, [Host, Port]).

send(Pid, Msg) ->
    Pid ! {send, Msg},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.

client(Host, Port) ->
    io:format("Client ~p connects to ~p:~p~n", [self(), Host, Port]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]),
    loop(Socket).

loop(Socket) ->
    receive
        {send, Msg} ->
            io:format("Client ~p send ~p~n", [self(), Msg]),
            Size = byte_size(Msg),
            Header = <<Size:16/integer>>,
            gen_tcp:send(Socket, <<Header/binary, Msg/binary>>),
            loop(Socket);
        {tcp, Socket, Msg} ->
            io:format("Client ~p got message: ~p~n", [self(), Msg]),
            loop(Socket);
        stop ->
            io:format("Client ~p closes connection and stops~n", [self()]),
            gen_tcp:close(Socket)
    after 200 ->
            loop(Socket)
    end.
```

Сервер сперва читает 2 байта, определяет размер данных и затем читает все данные:

```
{ok, Header} = gen_tcp:recv(Socket, 2),
<<Size:16/integer>> = Header,
{ok, Msg} = gen_tcp:recv(Socket, Size),
```

В коде сервера функции **start/0** и **start/1** не изменились, остальное немного поменялось:

```
server(Port) ->
    io:format("start server at port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, raw}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    timer:sleep(infinity),
    ok.

accept(Id, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket #~p, session started~n", [Id]),
    handle_connection(Id, ListenSocket, Socket).

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 2) of
        {ok, Header} -> <<Size:16/integer>> = Header,
                        {ok, Msg} = gen_tcp:recv(Socket, Size),
                        io:format("Socket #~p got message: ~p~n", [Id, Msg]),
                        gen_tcp:send(Socket, Msg),
                        handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.
```

Пример сессии со стороны клиента:

```
2> Pid = client2:start().
Client <0.40.0> connects to "localhost":1234
<0.40.0>
3> client2:send(Pid, <<"Hello">>).
Client <0.40.0> send <<"Hello">>
ok
Client <0.40.0> got message: <<"Hello">>
4> client2:send(Pid, <<"Hello again">>).
Client <0.40.0> send <<"Hello again">>
ok
Client <0.40.0> got message: <<"Hello again">>
5> client2:stop(Pid).
Client <0.40.0> closes connection and stops
ok
```

И со стороны сервера:

```
2> server2:start().
start server at port 1234
ok
Socket #1 wait for client
Socket #2 wait for client
Socket #3 wait for client
Socket #4 wait for client
Socket #5 wait for client
Socket #1, session started
Socket #1 got message: <<"Hello">>
Socket #1 got message: <<"Hello again">>
Socket #1, session closed
Socket #1 wait for client
```

Все это хорошо, но на самом деле нет необходимости вручную разбираться
с заголовочным пакетом.  Это уже реализовано в **gen_tcp**.
Нужно указать размер служебного пакета в настройках при открытии сокета
на стороне клиента:

```
{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, 2}]),
```

и на стороне сервера:

```
{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, 2}]),
```

и необходимость самому формировать и разбирать эти заголовки пропадает.

На стороне клиента упрощается отправка:

```
gen_tcp:send(Socket, Msg),
```

и на стороне сервера упрощается получение:

```
handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Msg} -> io:format("Socket #~p got message: ~p~n", [Id, Msg]),
                     gen_tcp:send(Socket, Msg),
                     handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.
```

Теперь при вызове **gen_tcp:recv/2** мы указываем Length = 0.
**gen_tcp** сам знает, сколько байт нужно прочитать из сокета.


### Работа с текстовыми протоколами

Кроме варианта со служебным заголовком, есть и другой подход.  Можно
читать из сокета по одному байту, пока не встретится специальный байт,
символизирующий конец пакета. Это может быть нулевой байт, или символ
перевода строки.

Такой вариант характерен для текстовых протоколов (SMTP, POP3, FTP).

Писать свою реализацию чтения из сокета нет необходимости,
все уже реализовано в **gen_tcp**. Нужно только указать в настройках
сокета вместо _{packet, 2}_ опцию _{packet, line}_.

```
{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}]),
```

В остальном код сервера остается без изменений. Но теперь мы можем
опять вернуться к telnet-клиенту.

```
 $ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
hello
hello
hello again
hello again
^]
telnet> quit
Connection closed.
```

TCP-сервер, текстовый протокол и  telnet-клиент нам понадобятся в курсовой работе.
