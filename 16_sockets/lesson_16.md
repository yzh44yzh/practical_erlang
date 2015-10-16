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

```erlang
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

```erlang
1> {ok, Socket} = gen_udp:open(2001, [binary, {active, true}]).
{ok,#Port<0.587>}
```

И пошлем сообщение с 1-й ноды на 2-ю:

```erlang
2> gen_udp:send(Socket, {127,0,0,1}, 2001, <<"Hello from 2000">>).
ok
```

Вызываем **gen_udp:send/4**, передаем сокет, адрес и порт получателя, и само сообщение.

Адрес может быть доменным именем в виде строки или атома, или адресом IPv4 в виде кортежа
из 4-х чисел, или адресом IPv6 в виде кортежа из 8 чисел.

На 2-й ноде убедимся, что сообщение пришло:

```erlang
2> flush().
Shell got {udp,#Port<0.587>,{127,0,0,1},2000,<<"Hello from 2000">>}
ok
```

Сообщение приходит в виде кортежа _{udp, Socket, SenderAddress, SenderPort, Packet}_.

Пошлем сообщение с 2-й ноды на 1-ю:

```erlang
3> gen_udp:send(Socket, {127,0,0,1}, 2000, <<"Hello from 2001">>).
ok
```

На 1-й ноде убедимся, что сообщение пришло:

```erlang
3> flush().
Shell got {udp,#Port<0.587>,{127,0,0,1},2001,<<"Hello from 2001">>}
ok
```

Как видим, тут все просто.


## Активный и пассивный режим сокета

TODO

И **gen_udp**, и **gen_tcp**, оба имеют одну важную настройку: режим работы с входящими данными.

Это может быть либо активный режим _{active, true}_, либо пассивный режим _{active, false}_.

В активном режиме поток получает входящие пакеты в виде сообщений в
своем почтовом ящике.  И их можно получить и обработать вызовом
receive, как любые другие сообщения.

Для udp сокета это сообщения вида:

```erlang
{udp, Socket, IP, InPortNo, Packet}
```

мы их уже видели:

```erlang
{udp,#Port<0.587>,{127,0,0,1},2001,<<"Hello from 2001">>}
```

Для tcp сокета аналогичные сообщения:

```erlang
{tcp, Socket, Packet}
```

Активный режим прост в использовании, но опасен тем, что клиент может
переполнить очередь сообщений потока, исчерпать память и обрушить ноду.
Поэтому рекомендуется пассивный режим.

В пассивном режиме данные нужно забрать самому
вызовами **gen_udp:recv/3** и **gen_tcp:recv/3**:

```erlang
gen_udp:recv(Socket, Length, Timeout) ->
        {ok, {Address, Port, Packet}} | {error, Reason}
gen_tcp:recv(Socket, Length, Timeout) -> {ok, Packet} | {error, Reason}
```

Здесь мы указываем, сколько байт данных хотим прочитать из
сокета. Если там есть эти данные, то мы получаем их сразу. Если нет,
то вызов блокируется, пока не придет достаточное к-во данных. Можно
указать Timeout, чтобы не блокировать поток надолго.

Причем gen_udp игнорирует Length, и непонятно, зачем он вообще задается. (TODO может в доке есть пояснение)
Просто если есть какие-то данные в сокете, то они и отдаются.
А если нет, то вызов блокируется.

Еще есть вариант **{active, once}**. В этом случае сокет запускается в
активном режиме, получает первый пакет данных как сообщение, и сразу
переключается в пассивный режим.


## TCP сокет

TODO: нужна какая-то вводная

http://www.erlang.org/doc/man/gen_tcp.html

stateful, connection-based

protocol takes care of
- handling lost packets,
- re-ordering them,
- maintaining isolated sessions between multiple senders and receivers

reliable exchange of information
but slower and heavier to set up

clients and servers are two entirely different things.

A client will behave with the following operations:
- connect
- send
- receive
- close socket

server:
- listen
- accept
- send
- receive
- close socket

С TCP сокетом немного сложнее. Сперва нужно начать прослушивание порта:

```erlang
gen_tcp:listen(Port, Options) -> {ok, ListenSocket} | {error, Reason}
```

Затем начать принимать на этом порту соединения для клиентов:

```erlang
accept(ListenSocket) -> {ok, Socket} | {error, Reason}
```

TODO нужно какое-то более красивое решение, без timer:sleep(infinity)
и подробнее его описать. Хотя у Фреда сделано так.

Вызов accept блокируется, пока не появится клиент, желающий подключиться.
И нам нужен отдельный поток на каждого клиента.

Самое простое решение, после listen запускать новый поток для accept.
И этот новый поток, получив соединение с клиентом, опять запускает
новый поток, ожидающий следующего клиента. А сам уходит в цикл для
обработки данных, приходящих от клиента:

```erlang
listen(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    spawn(?MODULE, accept, [ListenSocket]),
    timer:sleep(infinity), % поток-владелец сокета не должен завершаться
    ok.

accept(ListenSocket) ->
    {ok, _Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, accept, [ListenSocket]),
    handle().

handle() ->
    receive
        {tcp, Socket, Msg} ->
            io:format("handle ~p~n", [Msg]),
            gen_tcp:send(Socket, Msg),
            handle()
    end.
```

The idling (sleep infinity) is necessary because the listen socket is
bound to the process that opened it, so that one needs to remain alive
as long as we want to handle connections.

Ну или в пассивном режиме нужно самому читать данные из сокета:
```erlang
recv(Socket, Length, Timeout) -> {ok, Packet} | {error, Reason}
```

Попробуем подключиться telnet клиентом и потестить.

TODO
сессия telnet клиента
активный режим
показать, как короткое сообщение приходит одним пакетом,
а длинное двумя пакетами


## Бинарные и текстовые протоколы

TODO подробнее

Два вида протоколов: бинарные с размером впереди, текстовые с разделителем
опробовать оба через telnet

Бинарные обычно устроены по принципу {Tag, Length, Data}
Нужно прочитать заголовок, по нему определить, сколько данных читать дальше.
Размер заголовка 1,2,4 байта
Реализация вручную. Или готовые настройки в gen_tcp.
http://www.erlang.org/doc/man/erlang.html#decode_packet-3

Options
most options are going to be similar for all IP sockets
The TCP ones do have a few more specific options, including a connection backlog ({backlog, N}), keepalive sockets ({keepalive, true | false}), packet packaging ({packet, N}, where N is the length of each packet's header to be stripped and parsed for you), etc.

(Encoding the data is sometimes called marshaling, and decoding the data
is sometimes called demarshaling.)

Having opened a socket with the {packet,N} option, we don’t need to worry
about data fragmentation. The Erlang drivers will make sure that all fragment-
ed data messages are reassembled to the correct lengths before delivering
them to the application.
TODO Это понятно для активного режима, но не понято для пассивного

Примеры: ASN.1, BERT, Protocol Buffer, Thrift
TODO ссылки
https://ru.wikipedia.org/wiki/ASN.1
http://www.erlang.org/doc/apps/asn1/index.html

BERT ссылка на доку, и ссылка на сайт про bert, с либами для других языков
term_to_binary, binary_to_term
библиотеки для других языков?

Текстовые нужно читать побайтно и накапливать в буфере, пока не встретится символ
окончания пакета. Обычно 0 - zero byte.

Примеры: JSON, XML
данные могут быть сжаты, и тогда их нужно прочитать как бинарные, распаковать,
и потом интерпретировать

TODO напомнить про тип данных iolist()

TODO реализация примитивного текстового протокола, с разделителем по \n
пассивный режим (считывание по одному байту), накопление в буфере.
использовать line http://www.erlang.org/doc/man/erlang.html#decode_packet-3
сессия telnet клиента


## Ranch Acceptor Pool

TODO подробнее

Эта реализация работает, но не очень эффективно. Гораздо эффективнее заранее создать пул
из пары сотен процессов, которые будут висеть в gen_tcp:accept и ждать клиентских соединений.
Тогда установка соединений будет быстрее.

Это реализовано в Ranch Acceptor Pool

http://yzh44yzh.by/post/ranch.html

Надежный базис для построения своего TCP сервера. Поверх него построен cowboy :)

И я использовал в своих проектах.

TODO: пример использования
