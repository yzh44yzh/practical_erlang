## Key-Value структуры данных

## proplists

Модуль [proplists](http://www.erlang.org/doc/man/proplists.html).
Пару "ключ-значение" положить в кортеж, и такие кортежи собрать в список.

```erlang
1> PropList = [{key1, "Val1"}, {key2, 2}, {key3, true}].
[{key1,"Val1"},{key2,2},{key3,true}]
```

Сокращенный вариант для значений **true**.

```erlang
2> PropList2 = [{key1, "Val1"}, {key2, 2}, key3].
[{key1,"Val1"},{key2,2},key3]
```

С добавлением все просто, для этого используем оператор **cons**:

```erlang
3> PropList3 = [{key4, "Hello"} | PropList2].
[{key4,"Hello"},{key1,"Val1"},{key2,2},key3]
```

С изменением значения тоже просто, для этого опять используем оператор **cons** :)

```erlang
4> PropList4 = [{key1, "New val"} | PropList3].
[{key1,"New val"}, {key4,"Hello"}, {key1,"Val1"}, {key2,2}, key3]
```

В списке есть два ключа **key1**, но proplists такое разрешает.
Будет возвращаться первое от головы списка значение.

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

```erlang
14> proplists:delete(key1, PropList4).
[{key4,"Hello"},{key2,2},key3]
```

Операции поиска и удаления выполняются не за логарифмическое время, а за линейное.

proplists популярен и широко используется в проектах
для конфигурирования, для хранения различных настроек и опций.


## dict

Модуль [dict](http://www.erlang.org/doc/man/dict.html) мощнее и эффективнее.

полный CRUD API (Create, Read, Update, Delete) и некоторые функции сверх того.

```erlang
1> Dict = dict:new().
{dict,0,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}

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

```erlang
4> dict:to_list(Dict2).
[{key1,"val 1"}]
5> dict:to_list(Dict3).
[{key1,"val 1"},{key2,"val 2"}]
```

Обратная функция **from\_list/1** тоже есть:

Для изменения значения в словаре тоже используем функцию **store/3**

```erlang
7> Dict4 = dict:store(key1, "new val", Dict3).
8> dict:to_list(Dict4).
[{key1,"new val"},{key2,"val 2"}]
```

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

Как видим, у нас есть два разных подхода к ситуации, когда ключ не найден.

```erlang
13> Dict5 = dict:erase(key1, Dict4).
14> dict:to_list(Dict5).
[{key2,"val 2"}]
```

Есть возможность для одного ключа хранить несколько значений.

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

## orddict

Модуль [orddict](http://www.erlang.org/doc/man/orddict.html) аналогичен модулю dict.

Хранит ключи в сортированом виде.

В тех случаях, когда важна производительность, вы должны сами пробовать разные
структуры, и сами проводить бенчмарки на своих данных.


## gb_trees

Модуль [gb_trees](http://www.erlang.org/doc/man/gb_trees.html).

General Balanced Trees

Деревья -- эффективные структуры данных, которые обеспечивают доступ к
своим элементам за логарифмическое время. Если они сбалансированы.

Балансировка выполняется после каждого добавления нового элемента.
Но не выполняется при модификации и удалении элемента.

CRUD API

```erlang
1> T = gb_trees:empty().
{0,nil}
2> T2 = gb_trees:insert(key1, "value 1", T).
{1,{key1,"value 1",nil,nil}}
3> T3 = gb_trees:insert(key2, "value 2", T2).
{2,{key1,"value 1",nil,{key2,"value 2",nil,nil}}}
4> T4 = gb_trees:insert(key2, "value 2", T3).
** exception error: {key_exists,key2}
```

Модифицируем значения:

```erlang
5> T4 = gb_trees:update(key1, "new value", T3).
{2,{key1,"new value",nil,{key2,"value 2",nil,nil}}}
6> T5 = gb_trees:update(key777, "new value", T4).
** exception error
```

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

```erlang
12> gb_trees:get(key1, T6).
"new value"
13> gb_trees:get(some_key, T6).
** exception error
14> gb_trees:lookup(key1, T6).
{value,"new value"}
15> gb_trees:lookup(some_key, T6).
none
```

```erlang
16> gb_trees:delete(key1, T6).
{2,{key2,"new value",nil,{key777,"new value",nil,nil}}}
17> gb_trees:delete(some_key, T6).
** exception error
19> gb_trees:delete_any(key1, T6).
{2,{key2,"new value",nil,{key777,"new value",nil,nil}}}
20> gb_trees:delete_any(some_key, T6).
{3,
 {key1,"new value",nil,
       {key2,"new value",nil,{key777,"new value",nil,nil}}
```

В **gb_trees** есть **map/2**, но нету **filter** и **fold**.

```erlang
22> gb_trees:map(fun(Key, Value) -> string:to_upper(Value) end, T6).
{3,
 {key1,"NEW VALUE",nil,
       {key2,"NEW VALUE",nil,{key777,"NEW VALUE",nil,nil}}}}
```

**iterator/1** и **next/1**

**to\_list/1**

Первый вариант немного медленнее, но экономит память. Второй вариант
быстрее, но требует выделения лишней памяти.
