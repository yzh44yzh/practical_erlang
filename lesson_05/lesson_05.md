## Key-Value структуры данных

Было бы трудно пользоваться языком, в котором нет key-value структур
данных.  И в эрланг они есть. И поскольку они очень востребованы в
любом проекте, рассмотрим их подробнее.


## proplists

Самое простое, что можно придумать, это собрать пару "ключ-значение" в
кортеж, и положить такие кортежи в список.

```erlang
1> PropList = [{key1, "Val1"}, {key2, 2}, {key3, true}].
[{key1,"Val1"},{key2,2},{key3,true}]
```

Именно это и делает модуль [proplists](http://www.erlang.org/doc/man/proplists.html).

Только proplists еще позволяет пары, где значение **true** сокращать,
сохраняя вместо кортежа просто ключ.

```erlang
2> PropList2 = [{key1, "Val1"}, {key2, 2}, key3].
[{key1,"Val1"},{key2,2},key3]
```

АПИ модуля довольно странное. Есть несколько функций для извлечения
значения по ключу, есть функция для удаление значения. Но нет функций
для добавления и изменения значения.

Впрочем, с добавлением все просто. Для этого используем оператор **cons**:

```erlang
3> PropList3 = [{key4, "Hello"} | PropList2].
[{key4,"Hello"},{key1,"Val1"},{key2,2},key3]
```

С изменением значения тоже просто, для этого опять используем оператор **cons** :)

```erlang
4> PropList4 = [{key1, "New val"} | PropList3].
[{key1,"New val"},
 {key4,"Hello"},
 {key1,"Val1"},
 {key2,2},
 key3]
```

Тогда оказывается, что в списке есть два ключа **key1**, но proplists
такое разрешает. В этом случае будет возвращаться первое от головы
списка значение.

Для извлечения значения по ключу есть функции **get\_value/2**,
**get\_value/3** и **get\_all\_values/2**.

```erlang
5> proplists:get_value(key1, PropList4).
"New val"
6> proplists:get_value(key777, PropList4).
undefined
7> proplists:get_value(key777, PropList4, "default value").
"default value"
8> proplists:get_all_values(key1, PropList4).
["New val","Val1"]
9> proplists:get_all_values(key777, PropList4).
[]
```

Есть еще функции **lookup/2** и **lookup_all/2**, они отличаются тем,
что возвращают не значение, а кортеж ключ-значение.

```erlang
10> proplists:lookup(key1, PropList4).
{key1,"New val"}
11> proplists:lookup(key777, PropList4).
none
12> proplists:lookup_all(key1, PropList4).
[{key1,"New val"},{key1,"Val1"}]
13> proplists:lookup_all(key777, PropList4).
[]
```

Ну и функция **delete/2** удаляет значение из списка:

```erlang
14> proplists:delete(key1, PropList4).
[{key4,"Hello"},{key2,2},key3]
```

Понятно, что такая структура данных не очень эффективна.  Операции
поиска и удаления выполняются, конечно, не за логарифмическое время, а
за линейное. Несмотря на это, proplists популярен, и широко
используется в проектах. Обычно он используется для конфигурирования,
для хранения различных настроек и опций.

Ну и в других случаях, когда мы знаем, что ключей в наших данных будет
не много, не больше нескольких десятков, то смело берем proplists.
Ибо в этой ситуации его эффективность не важна.


## dict

Модуль [dict](http://www.erlang.org/doc/man/dict.html) мощнее и эффективнее.

Он уже предлагает полный **CRUD** (Create, Read, Update, Delete) API и некоторые
функции свех того.

Для начала словарь нужно создать:

```erlang
1> Dict = dict:new().
{dict,0,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}
```

Затем можно добавлять новые значения функцией **store/3**.

```erlang
2> Dict2 = dict:store(key1, "val 1", Dict).
{dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],
        [[key1,118,97,108,32,49]],
        [],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}
3> Dict3 = dict:store(key2, "val 2", Dict2).
{dict,2,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],
        [[key1,118,97,108,32,49]],
        [[key2,118,97,108,32,50]],
        [],[],[],[],[],[],[],[],[],[],[],[],[]}}}
```

Как мы видим внутреннее представление это структуры
довольно сложное, читать его в консоли и в логах неудобно.
Тут на помощью придет функция **to\_list/1**

```erlang
4> dict:to_list(Dict2).
[{key1,"val 1"}]
5> dict:to_list(Dict3).
[{key1,"val 1"},{key2,"val 2"}]
```

Она просто превращает dict в proplists, и в таком виде данные читаются
гораздо лучше.

Обратная функция **from\_list/1** тоже есть:

```erlang
6> dict:from_list([{key1, "val 1"}, {key2, "val 2"}]).
{dict,2,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],
        [[key1,118,97,108,32,49]],
        [[key2,118,97,108,32,50]],
        [],[],[],[],[],[],[],[],[],[],[],[],[]}}}
```

Для изменения значения в словаре тоже используем функцию **store/3**

```erlang
7> Dict4 = dict:store(key1, "new val", Dict3).
8> dict:to_list(Dict4).
[{key1,"new val"},{key2,"val 2"}]
```

Далее, у нас есть две функции для чтения значения по ключу:

```erlang
9> dict:fetch(key1, Dict4).
"new val"
10> dict:fetch(key777, Dict4).
** exception error: bad argument
11> dict:find(key1, Dict4).
{ok,"new val"}
12> dict:find(key777, Dict4).
error
```

Функция **fetch/2** возвращает значение, если ключ найден. Или бросает
исключение, если такого ключа нет.  Функция **find/2** возвращает
кортеж {ok, Val}, если ключ найден, или атом error, если ключа нет.

Как видим, у нас есть два разных подхода к ситуации, когда ключ не
найден.  Почему так, и какой подход в какой ситуации нужно
использовать, мы выясним на одном из последующих уроков, когда будем
изучать обработку ошибок.

Ну и для удаления ключа из словаря используем функцию **erase/2**:

```erlang
13> Dict5 = dict:erase(key1, Dict4).
14> dict:to_list(Dict5).
[{key2,"val 2"}]
```

Это полный CRUD API, но кроме него в модуле dict есть и другие интересные функции.

Есть возможность для одного ключа хранить несколько значений.  Для
этого значения добавляются функцией **append/3** или **append_list/3**:

```erlang
1> D = dict:new().
2> D2 = dict:append(key1, "value 1", D).
3> D3 = dict:append(key1, "value 2", D2).
4> dict:to_list(D3).
[{key1,["value 1","value 2"]}]
5> D4 = dict:append_list(key1, ["value 3", "value 4"], D3).
6> dict:to_list(D4).
[{key1,["value 1","value 2","value 3","value 4"]}]
```

Еще есть функции высшего порядка **map/2**, **filter/2**, **fold/3**.
Они аналогичны функциям модуля **lists**, но принимают на вход словарь,
и на выходе отдают словарь.

```erlang
1> D = dict:new().
2> D2 = dict:store(1, "Bob", D).
3> D3 = dict:store(2, "Bill", D2).
4> D4 = dict:store(3, "Helen", D3).
5> dict:to_list(D4).
[{3,"Helen"},{2,"Bill"},{1,"Bob"}]
```

```erlang
6> D5 = dict:map(fun(Key, Val) -> string:to_upper(Val) end, D4).
8> dict:to_list(D5).
[{3,"HELEN"},{2,"BILL"},{1,"BOB"}]
```

```erlang
9> D6 = dict:filter(fun(Key, Val) -> length(Val) > 3 end, D4).
10> dict:to_list(D6).
[{3,"Helen"},{2,"Bill"}]
```

```erlang
11> dict:fold(fun(Key, Val, {KeySum, AllVals}) -> {KeySum + Key, [Val | AllVals]} end, {0, []}, D4).
{6,["Helen","Bill","Bob"]}
```

Обратите внимание, не foldl, не foldr, а просто fold. Поскольку ключи
в словаре не подразумевают определенной последовательности, то и
направление свертки не имеет смысла.


## orddict

Модуль [orddict](http://www.erlang.org/doc/man/orddict.html)
аналогичен модулю dict, и предоставляет точно такие же функции. Но
хранит ключи в сортированом виде.  Это позволяет чуть быстрее
извлекать значения по ключу, но чуть медленее добавлять значения.

Фред Хеберт в своей книге [пишет](http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#key-value-stores)
что orddict эффективен для небольших структур, порядка 75 элементов.
Если так, то смысла использовать orddict нет, для такого набора данных
и proplists вполне годится.

Однако, выбирая структуру данных для своего проекта, в тех случаях,
когда важна производительность, вы должны сами пробовать разные
структуры. И сами проводить бенчмарки на своих данных.


## gb_trees

http://www.erlang.org/doc/man/gb_trees.html

General Balanced Trees, on the other hand, have a bunch more functions
leaving you more direct control over how the structure is to be used.

There are basically two modes for gb\_trees: the mode where you know
your structure in and out (I call this the 'smart mode'), and the mode
where you can't assume much about it (I call this one the 'naive
mode').

In naive mode, the functions are
gb\_trees:enter/3
gb\_trees:lookup/2
gb\_trees:delete_any/2.

The related smart functions are
gb\_trees:insert/3
gb\_trees:get/2
gb\_trees:update/3
gb\_trees:delete/2.

The disadvantage of 'naive' functions over 'smart' ones is that
because gb_trees are balanced trees, whenever you insert a new element
(or delete a bunch), it might be possible that the tree will need to
balance itself. This can take time and memory (even in useless checks
just to make sure). The 'smart' function all assume that the key is
present in the tree: this lets you skip all the safety checks and
results in faster times.

То есть, если мы предполагаем, что после изменений балансировать
дерево не нужно, то пользуемся smart функциями. А если нужно,
то пользуемся naive функциями. Но тогда при чем тут функции
чтения lookup и get?

Как насчет такого варианта:
нужно сделать несколько вставок. Мы не хотим перебалансировать
после каждой вставки, а хотим один раз, после всех вставок.
Можно сделать все ставки smart, и последнюю naive?
TODO: это нужно как-то выяснить

Oh and also note that while dicts have a fold function, gb_trees don't: they instead have an iterator function
There is also gb\_trees:map/2, which is always a nice thing when you need it.


(general balanced trees)

TODO пример CRUD операций
enter, insert, update
get, lookup
delete, delete\_any

TODO это нужно проверить
gb_trees хранит ключи, понятно, в дереве. Поэтому еще более быстрый поиск O(ln(n)), но добавление может оказаться медленным, из-за необходимости перебалансировать дерево.

deletion only does not rebalance the tree
balance/1 после многих удалений

## maps

Все выше -- реализации средствами языка, то есть, поверх списков и структур данных

Maps -- нативная реализация в виртуальной машине.

http://www.erlang.org/doc/man/maps.html

TODO прочитать Фреда
http://learnyousomeerlang.com/maps

TODO пример CRUD операций

new
put, update
find, get
remove

merge
fold, map

from\_list, to\_list

TODO разобраться, какой сахар работает, а какой задуман, но еще не работает.

Maps are associative collections of key-value pairs. The key can be any Erlang
term. In Perl and Ruby they are called hashes; in C++ and Java they are called
maps, in Lua they are called tables, and in Python they are called dictionaries.

maps to\_json, from\_json -- во как.


ets на следующем уроке
