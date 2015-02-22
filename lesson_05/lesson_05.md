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

Модуль [gb_trees](http://www.erlang.org/doc/man/gb_trees.html) предлагает еще одну key-value структуру данных.

gb_trees означает General Balanced Trees, то есть сбалансированное
дерево общего назначения.  Если вы изучали алгоритмы, то знаете, что
деревья -- эффективные структуры данных, которые обеспечивают доступ к
своим элементам за логарифмическое время.  Но для этого нужно, чтобы
глубина всех веток дерева была примерно одинаковая.  Значит деревья
должны уметь перемещать свои узлы из одной ветки в другую, то есть,
балансироваться.

Понятно, что балансировка сама по себе требует какого-то времени.
В **gb\_trees** она выполняется после каждого добавления нового элемента.
Но не выполняется при модификации и удалении элемента.

Модуль **gb\_trees** тоже предоставляет CRUD API и некоторые функции сверх того.

Создадим дерево:

```erlang
1> T = gb_trees:empty().
{0,nil}
```

Добавим в него значения:

```erlang
2> T2 = gb_trees:insert(key1, "value 1", T).
{1,{key1,"value 1",nil,nil}}
3> T3 = gb_trees:insert(key2, "value 2", T2).
{2,{key1,"value 1",nil,{key2,"value 2",nil,nil}}}
4> T4 = gb_trees:insert(key2, "value 2", T3).
** exception error: {key_exists,key2}
```

Как видим, функция **insert/3** не позволяет добавлять один и тот же
ключ дважды, бросает исключение в этой ситуации.

Модифицируем значения:

```erlang
5> T4 = gb_trees:update(key1, "new value", T3).
{2,{key1,"new value",nil,{key2,"value 2",nil,nil}}}
6> T5 = gb_trees:update(key777, "new value", T4).
** exception error: no function clause matching gb_trees:update_1(key777,"new value",nil) (gb_trees.erl, line 258)
```

Функция **update/3** бросает исключение, если ключа в дереве нет.

```erlang
7> T5 = gb_trees:enter(key777, "new value", T4).
{3,
 {key1,"new value",nil,
       {key2,"value 2",nil,{key777,"new value",nil,nil}}}}
8> T6 = gb_trees:enter(key2, "new value", T5).
{3,
 {key1,"new value",nil,
       {key2,"new value",nil,{key777,"new value",nil,nil}}}}
```

А функция **enter/3** исключений не бросает. Если ключа нет, она его
добавляет, а если ключ есть, то изменяет значение.

Теперь попробуем получить значения по ключу:

```erlang
12> gb_trees:get(key1, T6).
"new value"
13> gb_trees:get(some_key, T6).
** exception error: no function clause matching gb_trees:get_1(some_key,nil) (gb_trees.erl, line 239)
14> gb_trees:lookup(key1, T6).
{value,"new value"}
15> gb_trees:lookup(some_key, T6).
none
```

Здесь у нас две функции. **get/2** бросает исключение, если ключа в
дереве нет, а **lookup/2** возвращает либо кортеж **{value, Value}**, либо
атом **none**.

Ну и попробуем удалить ключ:

```erlang
16> gb_trees:delete(key1, T6).
{2,{key2,"new value",nil,{key777,"new value",nil,nil}}}
17> gb_trees:delete(some_key, T6).
** exception error: no function clause matching gb_trees:delete_1(some_key,nil) (gb_trees.erl, line 403)
19> gb_trees:delete_any(key1, T6).
{2,{key2,"new value",nil,{key777,"new value",nil,nil}}}
20> gb_trees:delete_any(some_key, T6).
{3,
 {key1,"new value",nil,
       {key2,"new value",nil,{key777,"new value",nil,nil}}
```

И опять у нас две функции, которые по-разному реагируют на отсутствие
ключа.  **delete/2** бросает исключение, а **delete_any/2** просто
возвращает дерево без изменений.

Мы уже видели это в модуле **dict**, и договорились, что разберем
необходимость таких вариантов поведения на одном из последующих
уроков, когда будем изучать обработку ошибок.

По CRUD API все, теперь дополнительные функции:

В **gb_trees** есть **map/2**, но нету **filter** и **fold**.

```erlang
22> gb_trees:map(fun(Key, Value) -> string:to_upper(Value) end, T6).
{3,
 {key1,"NEW VALUE",nil,
       {key2,"NEW VALUE",nil,{key777,"NEW VALUE",nil,nil}}}}
```

Есть функции **iterator/1** и **next/1**, которые позволяют организовать
обход дерева. В качестве альтернативы можно преобразовать дерево в
proplists с помощью **to\_list/1**, и делать обход списка.

Первый вариант немного медленнее, но экономит память. Второй вариант
быстрее, но требует выделения лишней памяти.

Кстати, функции **from_list/1** нету, есть только **from_orddict/1**.

На следущем уроке продолжим тему key-value структур данных, и
рассмотрим maps и ets таблицы.
