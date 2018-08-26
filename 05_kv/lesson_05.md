## Key-Value структуры данных

Было бы трудно пользоваться языком, в котором нет key-value структур
данных.  И в эрланг они есть. И поскольку они очень востребованы в
любом проекте, рассмотрим их подробнее.


## proplists

Самое простое, что можно придумать, это собрать пару "ключ-значение" в
кортеж, и положить такие кортежи в список.

```
1> PropList = [{key1, "Val1"}, {key2, 2}, {key3, true}].
[{key1,"Val1"},{key2,2},{key3,true}]
```

Именно это и делает модуль [proplists](http://www.erlang.org/doc/man/proplists.html).

Только proplists еще позволяет пары, где значение -- **true**,
сокращать, сохраняя вместо кортежа просто ключ.

```
2> PropList2 = [{key1, "Val1"}, {key2, 2}, key3].
[{key1,"Val1"},{key2,2},key3]
```

API модуля довольно странное. Есть несколько функций для извлечения
значения по ключу, есть функция для удаление значения. Но нет функций
для добавления и изменения значения.

Впрочем, с добавлением все просто. Для этого используем оператор **cons**:

```
3> PropList3 = [{key4, "Hello"} | PropList2].
[{key4,"Hello"},{key1,"Val1"},{key2,2},key3]
```

С изменением значения тоже просто, для этого опять используем оператор **cons** :)

```
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

```
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

```
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

```
14> proplists:delete(key1, PropList4).
[{key4,"Hello"},{key2,2},key3]
```

Понятно, что такая структура данных не очень эффективна.  Операции
поиска и удаления выполняются не за логарифмическое время, а за
линейное. Несмотря на это proplists популярен и широко используется в
проектах.

Обычно он используется для конфигурирования, для хранения различных
настроек и опций.  Ну и в других случаях, когда мы знаем, что ключей в
наших данных будет не много, не больше нескольких десятков, то смело
берем proplists.  Ибо в этой ситуации его эффективность не важна.


## dict, orddict, gb_trees

[dict](http://www.erlang.org/doc/man/dict.html)
[orddict](http://www.erlang.org/doc/man/orddict.html)
[gb_trees](http://www.erlang.org/doc/man/gb_trees.html)

|| свойства || proplists                  || dict, orddict          || gb\_trees                     ||
| new       |                             | new()                   | empty()                         |
| create    | [{k,v} | PL]                | store(K, V, D)          | insert(K, V, T), enter(K, V, T) |
| read      | proplists:get\_value(K, PL) | fetch(K, D), find(K, V) | get(K, T), lookup(K, T)         |
| update    | [{k,v} | PL]                | store(K, V, D)          | update(K, V, T), enter(K, V, T) |
| delete    | proplists:delete(K, PL)     | erase(K, D)             | delete(K, T), delete_any(K, T)  |
| map       | lists:map(F, PL)            | map(F, D)               | map(F, T)                       |
| filter    | lists:filter(F, PL)         | filter(F, D)            | -                               |
| fold      | lists:foldl(F, A, PL)       | fold(F, A, D)           | -                               |
| other     |                             | to\_list, from\_list    | iterator, next                  |


## maps

Все, описанные выше структуры данных: **proplists**, **dict**, **orddict**,
**gb_trees**, реализованы поверх списков и кортежей, то есть, средствами
самого языка эрланг.  Понятно, что эти реализации будут уступать по
эффективности аналогичным структурам в императивных языках.

В отличие от них, модуль [maps](http://www.erlang.org/doc/man/maps.html) реализован внутри виртуальной
машины, средствами языка С. Так что от него вполне можно ожидать
большей эффективности.

Давайте попробуем CRUD API.

Создание новой карты **maps:new/0**:

```
1> M = maps:new().
 #{}
```
тоже самое с синтаксическим сахаром:

```
3> M = #{key1 => "value 1", key2 => "value 2"}.
 #{key1 => "value 1",key2 => "value 2"}
```

Добавление новых и изменение существующих элементов **maps:put/3**:

```
2> M2 = maps:put(key1, "value 1", M).
 #{key1 => "value 1"}
3> M3 = maps:put(key2, "value 2", M2).
 #{key1 => "value 1",key2 => "value 2"}
5> M4 = maps:put(key2, "new value", M3).
 #{key1 => "value 1",key2 => "new value"}
```

тоже самое с синтаксическим сахаром:

```
1> M = #{key1 => "value 1", key2 => "value 2"}.
 #{key1 => "value 1",key2 => "value 2"}
2> M2 = M#{key3 => "value 3"}.
 #{key1 => "value 1",key2 => "value 2",key3 => "value 3"}
3> M3 = M2#{key3 => "new value"}.
 #{key1 => "value 1",key2 => "value 2",key3 => "new value"}
```

Изменение существующих элементов **maps:update/3**:

```
6> M5 = maps:update(key1, "new value", M4).
 #{key1 => "new value",key2 => "value 2"}
7> M6 = maps:update(key777, "new value", M5).
** exception error: bad argument
     in function  maps:update/3
        called as maps:update(key777,"new value",#{key1 => "new value",key2 => "value 2"})
```

тоже самое с синтаксическим сахаром:

```
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

**put** и **update** можно делать одновременно:

```
21> M4 = M#{key1 := "new value", key3 => "value 3"}.
 #{key1 => "new value",key2 => "value 2",key3 => "value 3"}
```

Как видим, изменять элемент можно и функцией **put** и функцией **update**.
Но в случае, если ключ отсутствует, то **put** добавляет новый элемент,
а **update** бросает исключение.

Получение элемента по ключу **maps:find/2**, **maps:get/2**:

```
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

Опять две функции с разным поведением в случае отсутствия ключа.

Сахар работает частично.  Обращение по ключу не работает, но
извлечение значений с помощью сопоставления с образцом работает:

```
1> M = #{key1 => "value 1", key2 => "value 2"}.
 #{key1 => "value 1",key2 => "value 2"}
2> M#{key1}.
* 3: syntax error before: '}'
3> #{key1 := Val} = M.
 #{key1 => "value 1",key2 => "value 2"}
4> Val.
"value 1"
```

Удаление элемента по ключу **maps:remove/2**:

```
12> maps:remove(key1, M5).
 #{key2 => "value 2"}
13> maps:remove(key777, M5).
 #{key1 => "new value",key2 => "value 2"}
```

А здесь только одна функция.

Помимо CRUD API еще есть функции высшего порядка **maps:map/2**, **maps:filter/2** и **maps:fold/3**.

```
1> M = #{key1 => "Bob", key2 => "Bill", key3 => "Helen"}.
#{key1 => "Bob",key2 => "Bill",key3 => "Helen"}

2> maps:map(fun(K, V) -> string:to_upper(V) end, M).
#{key1 => "BOB",key2 => "BILL",key3 => "HELEN"}

3> maps:filter(fun(K, V) -> K /= key1 andalso K /= key2 end, M).
#{key3 => "Helen"}

4> maps:fold(fun(K, V, Acc) -> [V | Acc] end, [], M).
["Helen","Bill","Bob"]
```

Еще есть полезная функция **maps:merge/2**, которая сливает две карты в одну:

```
1> M1 = #{key1 => "Val 1", key2 => "Val 2"}.
 #{key1 => "Val 1",key2 => "Val 2"}
2> M2 = #{key2 => "Val 222", key3 => "Val 3"}.
 #{key2 => "Val 222",key3 => "Val 3"}
3> maps:merge(M1, M2).
 #{key1 => "Val 1",key2 => "Val 222",key3 => "Val 3"}
4> maps:merge(M2, M1).
 #{key1 => "Val 1",key2 => "Val 2",key3 => "Val 3"}
```

В случае, если обе карты имеют одинаковый ключ, то значение берется из
карты, идущей вторым аргументом.
