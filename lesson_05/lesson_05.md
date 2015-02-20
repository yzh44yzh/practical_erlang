## Key-Value структуры данных

Было бы трудно пользоваться языком, в котором нет key-value структур данных.
И в эрланг они есть. И поскольку они очень востребованы в любом проекте,
рассмотрим их подробнее.


## proplist

http://www.erlang.org/doc/man/proplists.html

Property lists are ordinary lists containing entries in the form of either tuples, whose first elements are keys used for lookup and insertion, or atoms, which work as shorthand for tuples {Atom, true}.

Other terms are allowed in the lists, but are ignored by this module.

If there is more than one entry in a list for a certain key, the first occurrence normally overrides any later

The proplists module contains functions for working with property
lists. Property lists are ordinary lists containing entries in the
form of either tagged tuples, whose first elements are keys used for
lookup and insertion, or atoms (such as blah ), which is shorthand for
the tuple {blah, true} .

You'll notice there is no function to add or update an element of the
list. This shows how loosely defined proplists are as a data
structure. To get these functionalities, you must cons your element
manually ([NewElement|OldList])

and use functions such as lists:keyreplace/4.

often used to deal with configuration lists

CRUD:

%% add item:
List2 = [{Key, Value} | List]

%% get item:
Value = proplists:get_value(Key, List)

%% delete item:
List3 = proplists:delete(Key, List)

update как такового нет. Можно просто добавить элемент с тем же ключом в начало, и get_value будет возвращать его.

[{Key, Value} | List]

А можно добавить элемент с одновременным удалением по ключу

[{Key, Value} | proplists:delete(Key, List)]

Из этого следует, что в списке могут быть несколько значений с одним ключом

get_all_values
lookup

Используется как небольшое KV хранилище. Не больше нескольких десятков элементов. Часто для хранения и передачи настроек или опций.



## dict

dicts are proplists with a taste for formality.

TODO посмотреть сорцы, как они реализованы. Похоже там что-то поверх proplists

TODO прочитать доку
http://www.erlang.org/doc/man/dict.html

TODO чем отличается от других kv
TODO пример CRUD операций
TODO какие есть фишки

dict:store
dict:fold

dict:find/2 (when you do not know whether the key is in the dictionaries),
dict:fetch/2 (when you know it is there or that it must be there)

1> D = dict:new().
2> D2 = dict:append(1, "Bob", D).
3> D3 = dict:append(2, "Bill", D2).
4> dict:find(1, D3).
{ok,["Bob"]}
5> dict:find(2, D3).
{ok,["Bill"]}
6> dict:find(3, D3).
error
8> D4 = dict:append(1, "John", D3).
9> dict:find(1, D4).
{ok,["Bob","John"]}
10> D5 = dict:erase(1, D4).
11> dict:find(1, D5).
error
12> dict:to_list(D5).
[{2,["Bill"]}]
13> dict:fetch(2, D5).
["Bill"]
14> dict:fetch(1, D5).
    exception error: bad argument


## orddict

the whole list is sorted for faster average lookup

TODO прочитать доку
http://www.erlang.org/doc/man/orddict.html

TODO чем отличается от других kv
TODO пример CRUD операций
TODO какие есть фишки

Orddicts are a generally good compromise between complexity and
efficiency up to about 75 elements (see my benchmark). After that
amount, you should switch to different key-value stores.

There are basically two key-value structures/modules to deal with larger amounts of data: dicts and gb_trees.

Ну странно получается, 75 элементов -- мелочь. Ради них и не требуется сортировка.
Фред пишет, что dict эффективнее на большем к-ве элементов. Тогда нафига вообще нужен orddict?

TODO погонять самому бенчмарк Фреда
http://learnyousomeerlang.com/static/erlang/keyval_benchmark.erl


## gb_trees

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


TODO прочитать доку
http://www.erlang.org/doc/man/gb_trees.html

(general balanced trees)

TODO чем отличается от других kv
TODO пример CRUD операций
TODO какие есть фишки


TODO это нужно проверить
gb_trees хранит ключи, понятно, в дереве. Поэтому еще более быстрый поиск O(ln(n)), но добавление может оказаться медленным, из-за необходимости перебалансировать дерево.


## maps

Все выше -- реализации средствами языка, то есть, поверх списков и структур данных
(TODO заглянуть в сорцы dict и gb_trees, чтобы убедиться)
Maps -- нативная реализация в виртуальной машине.

TODO прочитать доку
http://www.erlang.org/doc/man/maps.html

TODO прочитать Фреда
http://learnyousomeerlang.com/maps

TODO чем отличается от других kv
TODO пример CRUD операций
TODO какие есть фишки


Maps are associative collections of key-value pairs. The key can be any Erlang
term. In Perl and Ruby they are called hashes; in C++ and Java they are called
maps, in Lua they are called tables, and in Python they are called dictionaries.

maps to\_json, from\_json -- во как.


## array

TODO прочитать доку
http://www.erlang.org/doc/man/array.html

TODO чем отличается от других kv
TODO пример CRUD операций
TODO какие есть фишки

But what about code that requires data structures with nothing but numeric keys? Well for that, there are arrays. They allow you to access elements with numerical indices and to fold over the whole structure while possibly ignoring undefined slots.

Erlang arrays, at the opposite of their imperative counterparts, are not able to have such things as constant-time insertion or lookup. Because they're usually slower than those in languages which support destructive assignment and that the style of programming done with Erlang doesn't necessary lend itself too well to arrays and matrices, they are rarely used in practice.

## Заключение

I would therefore say that your application's needs is what should
govern which key-value store to choose. Different factors such as how
much data you've got to store, what you need to do with it and whatnot
all have their importance. Measure, profile and benchmark to make
sure.

ets на следующем уроке
