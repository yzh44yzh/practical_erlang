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
стабильным, работа по нему еще идет, и в новых версиях **maps** будут
меняться.  Для разработчиков эрланг высокая производительность
**maps** не приоритет. Сперва планируется получить удобный и
правильный API.  Тем не менее, можно ожидать, что эффективность
**maps** в новых версиях эрланг будет повышаться.

first make it work, then make it beautiful, and only if you need to, make it fast

Помимо функций модуля, есть еще синтаксический сахар, похожий на сахар
для **records**. Это похожесть вносит некоторую путаницу. Разработчики
думают, что **maps** являются улучшеной версией **records** и должны
их заменить. Это не так, **maps** являются улучшенной версией
**dict**, и должны заменить **dict** и **proplists**. А **records**
вообще не являются key-value структурой, и имеют совсем другое
применение.

У Фреда есть глава
http://learnyousomeerlang.com/maps


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
map, fold
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

TODO еще про мапы есть в новом издании Армстронга, а новое издание
есть у меня на планшете. Где планшет? :)

## ets таблицы
