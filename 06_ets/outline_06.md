## Key-Value структуры данных, продолжение.

## maps

**proplists**, **dict**, **orddict**, **gb_trees** реализованы средствами эрланг

**maps** реализован внутри виртуальной машины, средствами языка С

В 17-й версии эрланг. И пока что не считается стабильным.

Синтаксический сахар, похожий на сахар для **records**.
**maps** должны заменить **dict** и **proplists**.

### CRUD API:

```erlang
M = maps:new().
M = #{key1 => "value 1", key2 => "value 2"}.

M3 = maps:put(key2, "value 2", M2).
M4 = maps:put(key2, "new value", M3).
M = #{key1 => "value 1", key2 => "value 2"}.
M2 = M#{key3 => "value 3"}.

M5 = maps:update(key1, "new value", M4).
maps:update(key777, "new value", M5). exception
M2 = M#{key1 := "new value"}.
M3 = M#{key77 := "new value"}. exception

M4 = M#{key1 := "new value", key3 => "value 3"}.

maps:get(key1, M5).
maps:get(key777, M5). exception
maps:get(key777, M5, "default value").

maps:find(key1, M5).
maps:find(key777, M5).

M#{key1}. syntax error
 #{key1 := Val} = M.

maps:remove(key1, M5).
maps:remove(key777, M5).
```

### Прочие

**maps:map/2** и **maps:fold/3**. Функции **filter** нету.

```erlang
M = #{key1 => "Bob", key2 => "Bill", key3 => "Helen"}.
maps:map(fun(K, V) -> string:to_upper(V) end, M).
maps:fold(fun(K, V, Acc) -> [V | Acc] end, [], M).
```

**maps:merge/2**, которая сливает две карты в одну:

```erlang
M1 = #{key1 => "Val 1", key2 => "Val 2"}.
M2 = #{key2 => "Val 222", key3 => "Val 3"}.
maps:merge(M1, M2).
maps:merge(M2, M1).
```


## ETS таблицы

Можно рассматривать как базу данных (альтернатива для **Memcached**, **Redis**)
Она рассчитана на хранение большого объема данных и быстрый доступ к ним.

Erlang Term Storage
Они реализованы на С как часть виртуальной машины, и очень эффективны по производительности.

Хранят кортежи произвольного размера.
Один из элементов которых используется как ключ.


### CRUD API

```erlang
MyEts = ets:new(my_ets, []).
```

Если мы создали ее прямо в консоли, то нужно быть осторожными.

```erlang
ets:insert(MyEts, {1, "Bob", 25}).
ets:insert(MyEts, [{2, "Bill", 30}, {3, "Helen", 22}]).

ets:lookup(MyEts, 1).
ets:lookup(MyEts, 4).

ets:insert(MyEts, {3, "Helen A.", 21}).

ets:delete(MyEts, 2).
```

### Настройки таблицы

Тип таблицы: **set**, **ordered\_set**, **bag**, **duplicate_bag**

Тип доступа: **public**, **protected**, **private**

Позиция ключа в кортеже: **{keypos, K}**


### Обход таблицы

first/1,next/2

last/1,prev/2

'$end_of_table'

ets:tab2list/1


### Выбор объектов по шаблону

**ets:match/2**

Шаблон: атомы вида '$1', '$2', '$3', либо конкретные значения.

```erlang
ets:match(T, {'$1', '$2', male}).
ets:match(T, {'$1', '$2', female}).
ets:match(T, {'$2', '$1', '_'}).

ets:match_object(T, {'$1', '_', male}).
ets:match_delete/2
```

**ets:select/2**

**спецификация совпадения** (match specification).

[Пример взят из книги Фреда Хеберта](http://learnyousomeerlang.com/ets#you-have-been-selected).

```erlang
[
{{'$1','$2',<<1>>,'$3','$4'},
[{'andalso',{'>','$4',150},{'<','$4',500}},
{'orelse',{'==','$2',meat},{'==','$2',dairy}}],
['$1']},
{{'$1','$2',<<1>>,'$3','$4'},
[{'<','$3',4.0},{is_float,'$3'}],
['$1']}
]
```

Первая часть называется **базовый шаблон** (Initial Pattern).

Вторая часть -- **гарды**.

Третья часть -- это значение, которое мы хотим вернуть.

Синтаксический сахар:

```erlang
fun({Food, Type, <<1>>, Price, Calories})
    when Calories > 150 andalso Calories < 500,
         Type == meat orelse Type == dairy;
         Price < 4.00, is_float(Price) ->
    Food
end.
```

Сахар, который с помощью ets:fun2ms/1 (fun to match specification) преобразуется в шаблон.

Пример в main.erl

С помощью переменной '$_' мы возвращаем весь кортеж.


### Что еще нужно знать о ETS таблицах

Не подвергаются сборке мусора.

Процесс, в котором таблица создана, является ее владельцем.

Вызовы **ets:insert/2** и **ets:delete/2** выполняются атомарно и изолированно.

По first/next, last/prev, match, select:
каждый кортеж будет рассмотрен, и рассмотрен только один раз
новые кортежи могут быть рассмотрены, а могут быть пропущены.


### DETS и Mnesia

DETS -- хранение на диске.

А поверх DETS построена полноценная база данных Mnesia:

 - распределенная
 - шардинг
 - сложный язык запросов
 - транзакции

DETS не может хранить больше 2Гб данных.

Если DETS падает с ошибкой, то восстановление данных может занять много времени.
