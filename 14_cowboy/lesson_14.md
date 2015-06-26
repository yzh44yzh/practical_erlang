# Cowboy

## Введение
http://ninenines.eu/docs/en/cowboy/1.0/guide/

Cowboy is a small, fast and modular HTTP server written in Erlang.

Complete HTTP stack:
- HTTP/1.0
- HTTP/1.1
- HTTPS
- WebSockets
- SPDY
- HTTP/2.0 (cowboy 2.0)

Cowboy:
- high quality (It includes hundreds of tests and its code is fully compliant with the Dialyzer)
- small code base
- very efficient (both in latency and memory use)
- can easily be embedded in another application
- well documented

https://github.com/ninenines/cowboy
Лоик Хоган и 76 контрибуторов


## Quick Start

http://ninenines.eu/docs/en/cowboy/1.0/guide/getting_started/
тут простой http хендлер
только erlang.mk вместо rebar

Но мы сделаем по-другому

Клонировать ковбой, переключиться на тэг 1.0.1, собрать проект:
```
git clone https://github.com/ninenines/cowboy
cd cowboy
git checkout -b 1.0.1 1.0.1
make
```

дальше quick\_start:
- try\_cowboy.erl
- index_handler.erl
- Makefile


## Роутинг и http хендлеры

http://ninenines.eu/docs/en/cowboy/1.0/guide/routing/

добавить еще один хендлер и в нем показать get, post, bindings:
- try\_routing.erl
- sample_handler.erl
- not_found_handler.erl


## Статика

http://ninenines.eu/docs/en/cowboy/1.0/guide/static_handlers/

```
{"/static/[...]", cowboy_static, {dir, "./static/"}}
```

page.html
lenin.jpg
