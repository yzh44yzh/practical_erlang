## Ссылочная прозрачность

**ссылочная прозрачность** (referential transparency).

Эрланг гарантирует, что выделенная память не модифицируется,
и значение, однажды присвоенное переменной, никогда не изменится.

Благодаря ссылочной прозрачности:

 - устраняется определенный класс ошибок;
 - упрощается отладка;
 - компилятор имеет больше возможностей для оптимизации кода;
 - статический анализатор имеет больше возможностей для проверки корректности кода.

Плата за это -- несколько менее эффективные структуры данных, чем в императивных языках.


## Устройство списков

```erlang
List = [1, 2, 3, 4].
```

![Linked list im memory](http://yzh44yzh.github.io/img/practical_erlang/list_1.png)

```erlang
List2 = [0 | List].
```

Оператор **|** называется **cons**, он добавляет новый элемент к
голове списка и возвращает новый список.

![List concatenation](http://yzh44yzh.github.io/img/practical_erlang/list_2.png)

Любой список можно представить как последовательность операторов cons:

```erlang
[0 | [1 | [2 | [3 | [4 | []]]]]]
```

![List as set of concatenations](http://yzh44yzh.github.io/img/practical_erlang/list_3.png)

```erlang
[Head | Tail] = List.
```

![Doubly linked list](http://yzh44yzh.github.io/img/practical_erlang/list_4.png)

И попробуем добавить к такому списку новое значение.

![Add element to doubly linked list](http://yzh44yzh.github.io/img/practical_erlang/list_5.png)

![Copy element of doubly linked list](http://yzh44yzh.github.io/img/practical_erlang/list_6.png)

И таким образом мы скопируем весь список целиком.

Двунаправленные связанные списки делают невозможным использование одного участка памяти несколькими списками.


## Стоимость операций для списков

 - добавить элемент в начало списка - O(1);
 - добавить элемент в конец списка - O(n) и полное копирование памяти;
 - определить длину списка - O(n);
 - получить N-й элемент - O(n).

Еще рассмотрим суммирование двух списков:

```erlang
List1 ++ List2.
```

Стоимость этой операции O(n), где n длинна первого списка.

Для сравнения, стоимость операций для массивов в императивных языках:

 - добавить элемент в начало массива - O(n), ибо все остальные элементы нужно сдвигать;
 - добавить элемент в конец массива - O(1), если мы укладываемся в размер массива, O(n) если массив нужно увеличивать;
 - определить длину массива - O(1);
 - получить/модифицировать N-й элемент - O(1).

Главное преимущество массивов над списками -- константное время доступа к любому элементу.
Цена за это: потеря ссылочной прозрачности.


## Рекурсивные функции с аккумуляторами

```erlang
Users = [{user, 1, "Bob", male},
         {user, 2, "Helen", female},
         {user, 3, "Bill", male},
         {user, 4, "Kate", female}].
```

Отфильтровать пользователей женского пола.

```erlang
filter_female([], Acc) -> Acc;
filter_female([User | Rest], Acc) ->
    case User of
        {user, _, _, male} -> filter_female(Rest, Acc);
        {user, _, _, female} -> filter_female(Rest, [User | Acc])
    end.
```

```erlang
filter_female([], Acc) -> lists:reverse(Acc);
```

Из списка пользователей нужно извлечь их идентификаторы и имена.

```erlang
get_names([], Acc) -> lists:reverse(Acc);
get_names([User | Rest], Acc) ->
    {user, Id, Name, _} = User,
    get_names(Rest, [{Id, Name} | Acc]).
```

Хорошая практика -- предлагать как публичный АПИ функцию с одним аргументом.

```erlang
get_names(Users) -> get_names(Users, []).

get_names([], Acc) -> lists:reverse(Acc);
get_names([User | Rest], Acc) ->
    {user, Id, Name, _} = User,
    get_names(Rest, [{Id, Name} | Acc]).
```

Пример со сложным аккумулятором.

```erlang
Users = [{user, 1, "Bob", male, 27},
         {user, 2, "Helen", female, 18},
         {user, 3, "Bill", male, 15},
         {user, 4, "Kate", female, 11}].
```

```erlang
partition_users(Users) -> partition_users(Users, {[], []}).

partition_users([], {List1, List2}) -> {lists:reverse(List1), lists:reverse(List2)};
partition_users([User | Rest], {List1, List2}) ->
    {user, _, _, _, Age} = User,
    if
        Age < 18 -> partition_users(Rest, {[User | List1], List2});
        true -> partition_users(Rest, {List1, [User | List2]})
    end.
```

## Хвостовая рекурсия

Нет циклов их заменяет рекурсия.

Каждый рекурсивный вызов требует сохранения на стеке
предыдущего состояния функции, чтобы вернуться в него после очередного
вызова.

И хотя стек в эрланге легковесный и позволяет делать миллионы рекурсивных
вызовов, он, все-таки, конечный.

Компилятор делает **оптимизацию хвостовой рекурсии**.

Это позволяет делать бесконечную рекурсию, которая нужна для
бесконечно живущих процессов.  А такие процессы нужны серверам :)
