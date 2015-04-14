# Строки

## string

Telecom applications do not rely on string operations, and as a result, strings were
never included as a data type in Erlang.

latin1 (ISO-8859-1) and unicode

Characters are represented by integers, and strings (of characters) are represented by
lists of integers

There is no string data type in Erlang. Strings are denoted by lists of ASCII values (latin1)
или юникод код-пониты (unicode)

```erlang
3> io:format("~w", ["hello"]).
[104,101,108,108,111]ok
4> io:format("~w", ["привет"]).
[1087,1088,1080,1074,1077,1090]ok
```

Every character in Erlang consumes 8 bytes in
the 32-bit emulator (and 16 in the 64-bit emulator), ensuring that characters and strings
are not stored in a memory-efficient way.

[модуль string](http://www.erlang.org/doc/man/string.html)

*tokens/2* -- разбивает сроку на подстроки по разделителю.
```erlang
1> string:tokens("http://google.com/?q=hello", "/").
["http:","google.com","?q=hello"]
```

Но тут есть один нюанс: второй аргумент, это список разделителей, а не подстрока.

```erlang
1> Str = "aa+bb-cc+-+dd".
"aa+bb-cc+-+dd"
2> string:tokens(Str, "+-").
["aa","bb","cc","dd"]
```

Если нужно разбиение по подстроке, то придется писать кастомную функцию.

*join/2* -- обратная по смыслу функция.

```erlang
`> string:join(["item1", "item2", "item3"], ", ").
"item1, item2, item3"
```

*strip* -- удаляет пробелы (или другие символы) в начале и/или конце строки.

```erlang
8> S2 = "    bla bla bla   ".
"    bla bla bla   "
9> string:strip(S2).
"bla bla bla"
10> string:strip(S2, left).
"bla bla bla   "
11> string:strip(S2, right).
"    bla bla bla"
12> string:strip(S2, both).
"bla bla bla"
13> string:strip("---bla-bla-bla----", both, $-).
"bla-bla-bla"
```

**to_upper**, **to_lower**

```erlang
19> string:to_upper("Hello").
"HELLO"
20> string:to_lower("Hello").
"hello"
21> string:to_upper("Привет").
"Привет"
22> string:to_lower("Привет").
```

Работает только с латинскими символами, остальные не меняет.
Позже я расскажу, как решать эту проблему.

**to_integer**, **to_float**

When matching strings, the following is a valid pattern:
f("prefix" ++ Str) -> ...
This is syntactic sugar for the equivalent, but harder to read
f([$p,$r,$e,$f,$i,$x | Str]) -> ...

Вообще поддержка строк в эрланг довольно слабая.


## binary

Erlang does include binaries, which we discuss in Chapter 9, and these are recommen-
ded for representing long strings, particularly if they are being transported by an
application rather than being analyzed in any way.

A binary is a reference to a chunk of raw, untyped memory.

Binaries are effective in mov-
ing large amounts of data, with BIFs provided for coding, decoding, and binary ma-
nipulation


A binary is a data structure designed for storing large quantities of raw data
in a space-efficient manner. The Erlang VM is optimized for the efficient input,
output, and message passing of binaries.
Binaries should be used whenever possible for storing the contents of large
quantities of unstructured data, for example large strings or the contents of
files

http://www.erlang.org/doc/man/binary.html

split_binary(Bin, Pos) -> {Bin1, Bin2}

byte_size(Bin) -> Size

match/3 matches/3 split/3 replace/3
тут даже больше возможностей, чем в модуле strings
все это работает с юникодом

```erlang
1> Str = <<"Привет мир!"/utf8>>.
<<"Привет мир!"/utf8>>
2> binary:split(Str, [<<" ">>]).
[<<"Привет"/utf8>>,<<"мир!"/utf8>>]
3> binary:split(Str, [<<" ">>, <<"и"/utf8>>]).
[<<"Пр"/utf8>>,<<"вет мир!"/utf8>>]
4> binary:split(Str, [<<"и"/utf8>>]).
[<<"Пр"/utf8>>,<<"вет мир!"/utf8>>]
5> binary:replace(Str, <<"мир"/utf8>>, <<"Боб"/utf8>>).
<<"Привет Боб!"/utf8>>
6> binary:match(Str, <<"мир"/utf8>>).
{13,6}
```

Как дополнительная тема:
BERT сериализация
External Term Format
term_to_binary, binary_to_term



## iolist

http://learnyousomeerlang.com/buckets-of-sockets#io-lists

не нужна конкатенация
не нужно переводить string и binary друг в друга

An iolist
is defined recursively as a list whose elements are integers in 0..255 ,
binaries, or iolists:

```erlang
[$H, $e, [$l, <<"lo">>, " "], [[["W","o"], <<"rl">>]] | [<<"d">>]].
```

тут четко определено, что 0-255
если там будут числа больше 255, то сокет их не пример
нужно делать unicode:characters_to_list

другой тип, который разрешает юникод кодпоинты, определен в модуле unicode
надо посмотреть, как он называется.

iolist принимают:
- TCP, UDP сокеты
- все функции модуля io
- все функции модуля file
- модуль re



```erlang
66> L1 = ["My name is ", Name].
["My name is ","Вася"]
67> L2 = ["SELECT name FROM ", Table, " WHERE id = ", integer_to_list(Id)].
["SELECT name FROM ","users"," WHERE id = ","5"]
```

iolist можно долго формировать из разных кусков, делая любую вложенность.
И уже после того, как все сформировано, одним вызовом lists:flatten
или unicode:characters_to_binary получить окончательный результат:

```erlang
68> lists:flatten(L1).
"My name is Вася"
69> unicode:characters_to_binary(L2).
<<"SELECT name FROM users WHERE id = 5">>
```


## unicode

[[https://www.youtube.com/watch?v=MijmeoH9LT4][Characters, Symbols and the Unicode Miracle - Computerphile]]

http://www.erlang.org/doc/man/unicode.html

```erlang
1> Str = "Привет мир!".
[1055,1088,1080,1074,1077,1090,32,1084,1080,1088,33]
2> Bin8 = <<"Привет мир!"/utf8>>.
<<208,159,209,128,208,184,208,178,208,181,209,130,32,208,
  188,208,184,209,128,33>>
3> Bin16 = <<"Привет мир!"/utf16>>.
<<4,31,4,64,4,56,4,50,4,53,4,66,0,32,4,60,4,56,4,64,0,33>>
4> Bin32 = <<"Привет мир!"/utf32>>.
<<0,0,4,31,0,0,4,64,0,0,4,56,0,0,4,50,0,0,4,53,0,0,4,66,0,
  0,0,32,0,...>>
```

Речь пойдет об Erlang 17. В более ранних версиях поведение отличается,
но мы не будем их рассматривать.

Кодировку для бинарников нужно указывать, для строк не нужно:

```erlang
<<"привет"/utf8>>
<<"привет"/utf16>>
<<"привет"/utf32>>
<<"привет">> % неправильно
"привет" % правильно
```

опция +pc unicode
показать с ней и без нее, как выглядит строка и бинарник

Суть в том, что в некоторых случаях Erlang применяет эвристику, пытаясь определить,
является ли данный список строкой, чтобы отобразить его соответствующим образом.
Если флаг не задан, то эвристика применяется только для строк в *latin1*, а
если задан, то и для строк в *unicode*.

Аргументы форматирования [io:format](http://www.erlang.org/doc/man/io.html#fwrite-1)
- *~w* -- показывает term как есть, без модификаций.
- *~p* -- применяет эвристику, пытаясь определить, является ли term строкой в latin1.
- *~tp* -- применяет эвристику, пытаясь определить, является ли term строкой в unicode.
- *~s* -- показывает term как строку в latin1.
- *~ts* -- показывает term как строку в unicode.

показать, как они работают для строк, и для структур данных с вложенными строками

```erlang
7> io:format("~w", [Str]).
<<208,159,209,128,208,184,208,178,208,181,209,130,32,208,188,208,184,209,128,33>>ok
8> io:format("~p", [Str]).
<<208,159,209,128,208,184,208,178,208,181,209,130,32,208,188,208,184,209,128,33>>ok
9> io:format("~ts", [Str]).
Привет мир!ok
10> T = {message, Str}.
{message,<<"Привет мир!"/utf8>>}
11> io:format("~tp", [T]).
{message,<<"Привет мир!"/utf8>>}ok
12> io:format("~p", [T]).
{message,<<208,159,209,128,208,184,208,178,208,181,209,130,32,208,188,208,184,
           209,128,33>>}ok
```

[Модуль unicode](http://www.erlang.org/doc/man/unicode.html)
unicode:characters_to_list.
unicode:binary_to_list.
не использовать
erlang:binary_to_list
erlang:list_to_binary

Библиотека ux
https://github.com/erlang-unicode/ux
to_upper, to_lower
sort
search

проблема нечуствительности к регистру

Рекомендации:
пусть данные остаются в binary, если это возможно (если их нужно просто передавать или хранить)
если нужно что-то делать с контентом, тогда unicode:characters_to_list
но не binary_to_list
