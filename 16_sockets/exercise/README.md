# Key-Value Storage сервис с текстовым протоколом

[memcached](http://memcached.org/) имеет текстовый протокол,
так что с ним можно работать через telnet-клиент.
Протокол описан [здесь](https://github.com/memcached/memcached/blob/master/doc/protocol.txt).

Мы реализуем похожий, но упрощенный сервис на базе tcp-сервера в пассивном режиме.


## Описание протокола

Чтобы сохранить значение, нужно использовать команду _SET key value_.
Она добавляет новое значение, если его нет, или обновляет его, если оно уже есть.
Сервер отвечает строкой _STORED_.

```
$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
SET my_key Hello World!
STORED
```

Чтобы получить значение по ключу, используем команду _GET key_.
Сервер отвечает двумя строками _VALUE key value\r\nEND_, если значение найдено,
либо строкой _NOT FOUND_.

```
GET my_key
VALUE my_key Hello World!
END
GET other_key
NOT FOUND
```

Команда _GETS key1 key2 keyN_ позволяет получить несколько значений сразу по списку ключей.

```
GETS my_key other_key
VALUE my_key Hello World!
VALUE other_key NOT FOUND
END
```

```
SET key1 value1
STORED
SET key2 value2
STORED
GETS my_key key1 key2
VALUE my_key Hello World!
VALUE key1 value1
VALUE key2 value2
END
```

Команда _DELETE key_ удаляет значение из хранилища. Сервер отвечает _DELETED_ или _NOT FOUND_.

```
DELETE key1
DELETED
DELETE key5
NOT FOUND
```

Команда _ADD key value_ добавляет значение, но только если оно не было добавлено раньше.
Сервер отвечает _STORED_ или _EXISTS_.

```
ADD key2 my value
EXISTS
ADD key3 my value
STORED
```

Команда _REPLACE key value_ обновляет значение, если оно есть в хранилище.
Сервер отвечает _STORED_ или _NOT FOUND_.

```
REPLACE key2 new value
STORED
REPLACE key5 new value
NOT FOUND
```

Команда _APPEND key value_ добавляет новые данные в конец уже существующих.
Сервер отвечает _STORED_ или _NOT FOUND_.

```
GET key2
VALUE key2 new value
END
APPEND key2 (appended data)
STORED
GET key2
VALUE key2 new value(appended data)
END
APPEND key5 more data
NOT FOUND
```

Команда _PREPEND key value_ добавляет новые данные в начало к уже существующим.
Сервер отвечает _STORED_ или _NOT FOUND_.

```
GET key3
VALUE key3 my value
END
PREPEND key3 data added before
STORED
GET key3
VALUE key3 data added beforemy value
END
```


## Нюансы реализации

Проект должен представлять собой OTP-приложение с названием _mcache_.
Тесты к проекту пытаются запустить именно это приложение:

```
application:ensure_all_started(mcache)
```

Потом тесты запускают одновременно несколько tcp-клиентов, и пытаются общаться
с сервисом, как если бы это делал telnet-клиент. Смотрите _test/mcache\_tests.erl_.


### Разделитель строк

telnet-клиент использует последовательность _\r\n_ как разделитель строк.
gen\_tcp в режиме _{packet, line}_ корректно обрабатывает такой разделитель,
так что вызовы:

```
gen_tcp:recv(Socket, 0)
```

Возвращают бинарные пакеты, содержащую одну строку, отправленную из telnet-клиента.
При формировании ответа также нужно добавлять _\r\n_.

```
gen_tcp:send(Socket, <<Reply/binary, "\r\n">>)
```

### Процесс -- владелец сокета

В теории к данному уроку использовался вызов

```
timer:sleep(infinity).
```

чтобы поток -- владелец сокета не завершался.

Теперь нужно придумать другой подход. Лучше всего, если владельцем сокета будет поток gen_server.


### Отладка

Прежде, чем запускать тест, рекомендуется отладить ваш сервис через telnet клиент.

Собрать код без запуска тестов можно командой:

```
make build
```

А запустить сервис командой:

```
make run
```

Если вы загляните в Makefile, то увидите, что _run_ описан как:

```
run:
	erl -pa ebin -s mcache_app start
```

Это значит, что у вас должна быть реализована функция _mcache\_app:start/0_, запускающая сервис.

После этого в отдельном терминале можно запустить telnet-клиент:

```
telnet localhost 1234
```

И дальше можно общаться с сервисом из telnet-клиента.

Тесты ожидают, что сервис будет работать на порту 1234. Если вы
почему-то захотите использовать другой порт, то поправьте тесты.

После остановки сервиса операционная система может не сразу освободить
порт. И повторный запуск сервиса может оказаться неудачным -- ОС сообщит,
что порт уже занят. В этом случае добавьте опцию _{reuseaddr, true}_ в
параметры _gen\_tcp:listen/2_.
