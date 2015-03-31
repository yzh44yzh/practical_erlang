# Строки

TODO источники инфы:
  + notes.org
  + erlang-school
  - Армстронг
  - Цезарини
  - Хеберт
  - OTP in action
  - yzh44yzh.by есть пост про юникод, взять из него важное
  - официальные доки


## unicode

http://www.erlang.org/doc/man/unicode.html

Начать надо с примера строки "Привет мир" или типа того.
как она выглядить в код-поинтах, в utf8, utf16, utf32


## string

http://www.erlang.org/doc/man/string.html

We’ll finish this section with a short discussion of the history of strings in the Erlang system.
цезарини, 27-я страница


latin1
unicode

When matching strings, the following is a valid pattern:
f("prefix" ++ Str) -> ...
This is syntactic sugar for the equivalent, but harder to read
f([$p,$r,$e,$f,$i,$x | Str]) -> ...


## binary

http://www.erlang.org/doc/man/binary.html

можно взять примеры из term_to_binary, binary_to_term
но это не лучшие примеры


## io_list

TODO есть у Фреда, надо прочитать, и дать ссылку


## ???

модуль unicode

библиотека ux
https://github.com/erlang-unicode/ux

проблема нечуствительности к регистру

Рекомендации:
пусть данные остаются в binary, если это возможно (если их нужно просто передавать или хранить)
если нужно что-то делать с контентом, тогда unicode:characters_to_list
но не binary_to_list
