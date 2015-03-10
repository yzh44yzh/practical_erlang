## Key-Value структуры данных, продолжение.

## maps

Все, описанные выше структуры данных: **proplists**, **dict**, **orddict**,
**gb_trees**, реализованы поверх списков и кортежей, то есть, средствами
самого языка эрланг.  Понятно, что эти реализации будут уступать по
эффективности аналогичным структурам в императивных языках.

В отличие от них, модуль [maps](http://www.erlang.org/doc/man/maps.html) реализован внутри виртуальной
машины, средствами языка С. Так что от него вполне можно ожидать
большей эффективности.

Модуль появился недавно, в 17-й версии эрланг. И не считается пока что
стабильным. Работа по нему еще идет, и в новых версиях **maps** будут
меняться.  Для разработчиков языка высокая производительность
**maps** не приоритет. Сперва планируется получить удобный и
правильный API.  Тем не менее, можно ожидать, что эффективность
**maps** в новых версиях эрланг будет повышаться.

Свои приоритеты разработчики формулируют так:
"first make it work, then make it beautiful, and only if you need to, make it fast".
Формула хорошая, годится для большинства проектов :)
Есть более короткая формулировка, которая нравится мне больше:
"make it run, make it right, make it quick".

Помимо функций модуля, есть еще синтаксический сахар, похожий на сахар
для **records**. Это похожесть вносит некоторую путаницу. Разработчики
думают, что **maps** являются улучшеной версией **records** и должны
их заменить. Это не так, **maps** являются улучшенной версией
**dict**, и должны заменить **dict** и **proplists**. А **records**
вообще не являются key-value структурой, и имеют совсем другое
применение.

Из-за их новизны, maps не описаны в книгах. Только у Фреда Хеберта
есть [отдельная глава](http://learnyousomeerlang.com/maps), добавленная позже в онлайн версию книги.
Но она отсутствует в бумажной версии.


TODO пример CRUD операций

new/0

```erlang
1> M = maps:new().
 #{}
```

тож самое синтаксическим сахаром

```erlang
3> M = #{key1 => "value 1", key2 => "value 2"}.
 #{key1 => "value 1",key2 => "value 2"}
```


put/3

```erlang
2> M2 = maps:put(key1, "value 1", M).
 #{key1 => "value 1"}
3> M3 = maps:put(key2, "value 2", M2).
 #{key1 => "value 1",key2 => "value 2"}
5> M4 = maps:put(key2, "new value", M3).
 #{key1 => "value 1",key2 => "new value"}
```

с сахаром

```erlang
1> M = #{key1 => "value 1", key2 => "value 2"}.
 #{key1 => "value 1",key2 => "value 2"}
2> M2 = M#{key3 => "value 3"}.
 #{key1 => "value 1",key2 => "value 2",key3 => "value 3"}
3> M3 = M2#{key3 => "new value"}.
 #{key1 => "value 1",key2 => "value 2",key3 => "new value"}
```


update/3

```erlang
6> M5 = maps:update(key1, "new value", M4).
 #{key1 => "new value",key2 => "value 2"}
7> M6 = maps:update(key777, "new value", M5).
** exception error: bad argument
     in function  maps:update/3
        called as maps:update(key777,"new value",#{key1 => "new value",key2 => "value 2"})
```

с сахаром

```erlang
1> M = #{key1 => "value 1", key2 => "value 2"}.
 #{key1 => "value 1",key2 => "value 2"}
2> M2 = M#{key1 := "new value"}.
 #{key1 => "new value",key2 => "value 2"}
3> M3 = M#{key77 := "new value"}.
** exception error: bad argument
     in function  maps:update/3
        called as maps:update(key77,"new value",#{key1 => "value 1",key2 => "value 2"})
     in call from erl_eval:'-expr/5-fun-0-'/2 (erl_eval.erl, line 255)
     in call from lists:foldl/3 (lists.erl, line 1261)
```

кстати, put и update можно делать одновременно

```erlang
21> M4 = M#{key1 := "new value", key3 => "value 3"}.
 #{key1 => "new value",key2 => "value 2",key3 => "value 3"}
```

find/2, get/2

```erlang
8> maps:get(key1, M5).
"new value"
9> maps:get(key777, M5).
** exception error: bad_key
     in function  maps:get/2
        called as maps:get(key777,#{key1 => "new value",key2 => "value 2"})
10> maps:get(key777, M5, "default value").
"default value"
11> maps:find(key1, M5).
{ok,"new value"}
12> maps:find(key777, M5).
error
```

Сахар работает частично.  Обращение по ключу не работает, но
извлечение значений с помощью сопоставления с образцом работает.

```erlang
1> M = #{key1 => "value 1", key2 => "value 2"}.
 #{key1 => "value 1",key2 => "value 2"}
2> M#{key1}.
* 3: syntax error before: '}'
3> #{key1 := Val} = M.
 #{key1 => "value 1",key2 => "value 2"}
4> Val.
"value 1"
```

remove/2

```erlang
12> maps:remove(key1, M5).
 #{key2 => "value 2"}
13> maps:remove(key777, M5).
 #{key1 => "new value",key2 => "value 2"}
```

TODO
map, fold есть
filter нету

TODO
merge
from\_list, to\_list

TODO
maps comprehention заявлен, но пока не работает
```erlang
9> Weather = #{toronto => rain, montreal => storms, london => fog,
9>             paris => sun, boston => fog, vancouver => snow}.
10> FoggyPlaces = [X || X := fog <- Weather].
[london,boston]
```

## ets таблицы

Erlang Term Storage

это больше, чем kv структура данных, это in memory база данных
(memcached, redis)

императивный мирок внутри функционального языка:
- модифицируемые данных
- разделяемая между процессами память
- очень эффективны по производительности

Реализованы на С как часть виртуальной машины.
Они нарушают ссылочную прозрачность и изоляцию памяти ради производительности.
Могут хранить большие объемы данных, дают доступ за логарифмическое время.
Это пример того, что эрланг -- язык прагматичный, а не академический :)

Могут хранить кортежи произвольного размера, один из элементов которых используется как ключ.
По умолчанию -- первый. Но в настройках ETS можно указать позицию элемента-ключа.
И это нужно делать, если мы будем хранить там records (что часто бывает).

если юзать в консоли, и ошибка, процесс консоли падает и перезапускается
а ets теряется. Поэтому в консоли юзать не удобно, приходится аккуратно следить за опечатками
лучше пробовать сразу в модуле.

примеры CRUD

```erlang
1> Ets = ets:new(my_ets, [set, protected]).
16400
2> ets:insert(Ets, {1, "Bob", 25}).
true
3> ets:insert(Ets, [{2, "Bill", 30}, {3, "Helen", 22}]).
true
4> ets:lookup(Ets, 1).
[{1,"Bob",25}] -- внимание, возвращается массив значений, а не одно значение. Даже если это set.
5> ets:lookup(Ets, 3).
[{3,"Helen",22}]
6> ets:lookup(Ets, 4).
[]
7> ets:insert(Ets, {3, "Helen A.", 21}).
true
8> ets:lookup(Ets, 3).
[{3,"Helen A.",21}]
9> ets:delete(Ets, 2).
true
10> ets:lookup(Ets, 2).
[]
```erlang

When the table is created, it
has a set of options that cannot be changed.

set -- ключи должны быть уникальны
ordered\_set -- ключи должны быть уникальны, кортежи хранятся в сортированном виде
bag, -- разрешает одинаковые ключи, но значения должны быть разными
duplicate\_bag -- разрешает одинаковые значения

TODO попробовать добавление одинаковых ключей в set, что будет? Обновится значение?
insertion of a second element with the alison
key causes the first element to be overwritten.

Internally, ETS tables are represented by hash tables (except ordered
sets, which are represented by balanced binary trees).
This means there
is a slight space penalty for using sets and a time penalty for using
ordered sets. Inserting into sets takes place in constant time, but in-
serting into an ordered set takes place in a time proportional to the log
of the number of entries in the table.
Bags are more expensive to use than duplicate bags, since on each
insertion all elements with the same key have to be compared for equal-
ity.

public,
protected,
private

named_table
{keypos, K}

[set,protected,{keypos,1}] -- по дефолту

Обход таблицы:

ets:tab2list/1

```erlang
11> F = ets:first(Ets).
3
12> N1 = ets:next(Ets, F).
1
13> N2 = ets:next(Ets, N1).
'$end_of_table'
```

Самый эффективный способ выбрать группу объектов (или даже все):
```erlang
17> ets:match(Ets, {'$1', '_', '_'}).
[[1],[3]]
18> ets:match(Ets, {'$1', '$2', '_'}).
[[1,"Bob"],[3,"Helen A."]]
19> ets:match(Ets, '$1').
[[{1,"Bob",25}],[{3,"Helen A.",21}]]
```

ets:select, match spec, fun2ms

obsever:start, посмотреть таблицу

Память:
не подвергаются сборке мусора
удалять данные из них нужно явно
удаляется вся таблица при завершении процесса-родителя

An ETS table is said to be owned by the
process that created it—when that process dies or when ets:delete is
called, then the table is deleted.

Особенности concurrency:

Добавление, удаление, обновление одиночного объекта атомарно и изолировано.

Атомарно, это значит транзация либо завершится успешно, либо будет отменена.
Изолировано, это значит, что во время действия транзации ее промежуточные
результаты не будут видны другим процессам.

Однако при обходе таблицы с помощью first/next гарантий нет. Если во время такого обхода
таблица будет модифицироваться, то возможно, будут пропущены некоторые объекты.

This is because all match operations
are implemented as BIFs, and BIFs are executed atomically; a match
operation on a large table can therefore stop other processes from exe-
cuting until the operation has traversed the whole table.

Прочитанное из таблицы значение скопировалось в память процесса-читателя.
И последующие изменения в таблице не повлияют на копию в памяти процесса-читателя.

### dets, mnesia

dest добавляет хранение данных на диске
Disk ETS

Data stored in DETS tables is persistent and should survive an entire system crash.
(правда восстановление таблицы после краша может занять долгое время)
Since the repair can take a long time, it’s important to
close them properly before finishing your application.

DETS files have a maximum size of 2GB.

mnesia -- распределенное KV хранилище с поддержкой транзакций.
Ее никто не использует, кроме Ericsson. Предпочитают другие базы данных, например Riak.
