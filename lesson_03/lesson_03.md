## Функции высшего порядка

Функции в эрланг являются обычными значениями. Их можно сохранять в
переменную, передавать в аргументах, возвращать из функции.  Это еще
одна штука, которая отличает функциональные языки от императивных.

Функции высшего порядка (higher-order functions) -- звучит круто, но
ничего особо хитрого здесь нет.  Это всего лишь функции, которые
принимают в аргументах другие функции, или возвращают другие функции.
В упражнении ко второму уроку вы уже реализовали несколько таких.

lists:all/2, lists:any/2, lists:filter/2, lists:dropwhile/2 и т.д. --
все они принимают первым аргументом предикат, который является функцией.

В модуле lists много функций высшего порядка. И самые ходовые из них,
это lists:map/2 и lists:filter/2.

**map** применяет переданную функцию к каждому элементу списка и возвращает
новый список.

```erlang
1> List = [1,2,3,4,5].
[1,2,3,4,5]
2> F = fun(Val) -> Val * 2 end.
 #Fun<erl_eval.6.90072148>
3> lists:map(F, List).
[2,4,6,8,10]
```

```erlang
4> List2 = [{user, 1, "Bob"}, {user, 2, "Bill"}, {user, 3, "Helen"}].
[{user,1,"Bob"},{user,2,"Bill"},{user,3,"Helen"}]
5> F2 = fun({user, Id, Name}) -> {user, Id, string:to_upper(Name)} end.
 #Fun<erl_eval.6.90072148>
6> lists:map(F2, List2).
[{user,1,"BOB"},{user,2,"BILL"},{user,3,"HELEN"}]
```

**filter** использует переданную функцию как предикат для фильтрации списка.

```erlang
7> lists:filter(fun(Val) -> Val > 3 end, List).
[4,5]
8> lists:filter(fun({user, Id, _}) -> Id rem 2 =:= 0 end, List2).
[{user,2,"Bill"}]
```

Мы можем взять примеры из прошлого урока, и переписать их с использованием map и filter.

Вспомним список пользователей, с которым мы работали:

```erlang
get_users() ->
    [{user, 1, "Bob", male, 22},
     {user, 2, "Helen", female, 14},
     {user, 3, "Bill", male, 11},
     {user, 4, "Kate", female, 18}].
```

Фильтрация пользователей по полу:

```erlang
get_females(Users) ->
    F = fun({user, _, _, male, _}) -> false;
           ({user, _, _, female, _}) -> true
        end,
    lists:filter(F, Users).
```

Получение id и name пользователя:

```erlang
get_id_name(Users) ->
    F = fun({user, Id, Name, _, _}) -> {Id, Name} end,
    lists:map(F, Users).
```

Если нам нужно сделать и **map**, и **filter**, то мы можем применить
их по очереди:

```erlang
get_females_id_name(Users) ->
    Users2 = lists:filter(fun({user, _, _, Gender, _}) -> Gender =:= female end, Users),
    lists:map(fun({user, Id, Name, _, _}) -> {Id, Name} end, Users2).
```

Но так мы получим 2 прохода по списку. Можно сделать это в один проход,
если воспользоваться функцией [lists:filtermap/2](http://www.erlang.org/doc/man/lists.html#filtermap-2).

```erlang
get_females_id_name2(Users) ->
    lists:filtermap(fun({user, _, _, male, _}) -> false;
                       ({user, Id, Name, female, _}) -> {true, {Id, Name}}
                    end, Users).
```

Есть много примеров, где функция передается аргументом в другую функцию.
Но довольно редко бывает, чтобы функция возвращалась как значение.
Учебные примеры в книгах вы найдете. А что насчет применения на практике, в реальной работе?
Кое что есть :)

В **EUnit**, фреймворке для модульного тестирования, используются генераторы юнит тестов.
Они возвращают список функций.

Бывает, что один модуль запрашивает у другого функцию обратного вызова (callback),
чтобы сохранить ее у себя и потом, при каких-то условиях, вызвать. Второй модуль
может определить для этого API-функцию, которая вернет callback-функцию.

Примеров мало, и это не повседневная практика, а какие-то особые случаи.

Еще с помощью функций, возвращающих функции, можно реализовать ленивые вычисления.
Кому интересно, почитайте об этом в 9-й главе книги Чезарини.


## Свертка

Еще одна важная штука в функциональном программировании. Ее понять немного сложнее,
чем map и filter. Ну давайте разберемся.

Map принимает список, и возвращает список. Свертка принимает список, и возвращает
одно значение -- "сворачивает" список.

[lists:foldl](http://www.erlang.org/doc/man/lists.html#foldl-3) принимает 3 аргумента:

 - функцию сворачивания
 - начальное значение аккумулятора
 - список

Функция сворачивания принимает 2 аргумента: текущий элемент списка и текущее значение
аккумулятора. И должна вернуть новое значение аккумулятора.

Классический пример -- суммирование и произведение элементов списка:

```erlang
1> List = [1,2,3,4,5].
[1,2,3,4,5]
2> lists:foldl(fun(Item, Acc) -> Acc + Item end, 0, List).
15
3> lists:foldl(fun(Item, Acc) -> Acc * Item end, 1, List).
120
```

Для суммирования начальное значение аккумулятора 0, и потом к нему прибавляется каждый элемент списка.
Для произведения начальное значение аккумулятора 1, и потом на него умножается каждый элемент списка.

Но это учебные примеры, они не интересные :) Давайте сделаем что-нибудь интересное с нашим списком пользователей:
```erlang
get_users() ->
    [{user, 1, "Bob", male, 22},
     {user, 2, "Helen", female, 14},
     {user, 3, "Bill", male, 11},
     {user, 4, "Kate", female, 18}].
```

Соберем полезную статистику по пользователям: сколько в списке мужчин, сколько женщин,
и средний возраст пользователей.

В качестве аккумулятора возьмем кортеж из 4х чисел:

```erlang
{Males, Females, TotalUsers, TotalAge}
```

Как результат свертки получим этот кортеж, заполненный актуальными данными.

Реализуем:
```erlang
get_stat(Users) ->
    F = fun({user, _, _, Gender, Age}, {Males, Females, TotalUsers, TotalAge}) ->
                case Gender of
                    male -> {Males + 1, Females, TotalUsers + 1, TotalAge + Age};
                    female -> {Males, Females + 1, TotalUsers + 1, TotalAge + Age}
                end
        end,
    lists:foldl(F, {0, 0, 0, 0}, Users).
```

Как видим, тут главное написать сворачивающую функцию, которая пойдет первым аргументом
в lists:foldl, и правильно задать начальное значение аккумулятора.

Пробуем применить:
```erlang
1> Users = main:get_users().
[{user,1,"Bob",male,22},
 {user,2,"Helen",female,14},
 {user,3,"Bill",male,11},
 {user,4,"Kate",female,18}]
2> {M, F, TU, TA} = main:get_stat(Users).
{2,2,4,65}
```

Средний возраст сразу не получим, но получим данные, чтобы его расчитать.

```erlang
3> TA / TU.
16.25
```

На прошлом уроке мы изучали рекурсивные функции с аккумуляторами.
Полезно знать, что все, что можно реализовать таким образом, можно
реализовать и через свертку.

Например, мы делили список пользователей по возрасту на два списка: тех, кому меньше 18, и взрослых:

```erlang
split_by_age(Users) -> split_by_age(Users, {[], []}).

split_by_age([], {Acc1, Acc2}) -> {lists:reverse(Acc1), lists:reverse(Acc2)};

split_by_age([User | Rest], {Acc1, Acc2}) ->
    {user, _, _, _, Age} = User,
    if
        Age < 18 -> split_by_age(Rest, {[User | Acc1], Acc2});
        true -> split_by_age(Rest, {Acc1, [User | Acc2]})
    end.
```

Вот как можно сделать тоже самое через свертку:

```erlang
split_by_age(Users) ->
    lists:foldl(fun(User, {Acc1, Acc2}) ->
                        {user, _, _, _, Age} = User,
                        if
                            Age < 18 -> {[User | Acc1], Acc2};
                            true -> {Acc1, [User | Acc2]}
                        end
                end,
                {[], []},
                Users).
```

Этот пример может показаться менее понятным, чем предыдущий.  Но это
дело привычки. Между тем, рекомендуется применять именно свертку
вместо кастомных рекурсивных функций. Выше уровень абстрации, значит
меньше кода.  Меньше кода, значит меньше возможностей допустить
ошибку.

Теперь понятно, почему на прошлом уроке мы сделали один составной
аккумулятор, вместо 2-х простых. Это было для того, чтобы перейти к
свертке :)

Свертка бывает левая и правая: lists:foldl/3, lists:foldr/3. Левая
начинает от головы списка и идет к хвосту. Правая начинает от хвоста и
идет к голове. Они могут отличаться результатом, а могут не
отличаться, смотря как реализована сворачивающая функция.  Но важно
знать, что левая свертка реализована с хвостовой рекурсией, а правая с
обычной рекурсией, с ростом стека и возвратом назад по стеку.


## Конструкторы списков

lists comprehention

List comprehension is another powerful construct whose roots lie in functional pro-
gramming. List comprehension constructs allow you to generate lists, merge them to-
gether, and filter the results based on a set of predicates. The result is a list of elements
derived from the generators for which the predicate evaluated to true.

Синтаксис взять из математики

```
{x | x ∈ N, x > 0}
```
all values x such that x comes from the
natural numbers (denoted by N), and x is greater than zero

```erlang
[X || X <- ListOfIntegers, X > 0]
```

```erlang
L = [1,2,3,4,5].
lists:map(fun(X) -> 2*X end, L).
[2*X || X <- L ].
```

[ Expression || Generators, Guards, Generators, ... ]
Generators
A generator has the form Pattern <- List , where Pattern is a pattern that is
matched with elements from the List expression. You can read the symbol <- as
“comes from”; it’s also like the mathematical symbol ∈, meaning “is an element of.”
Guards
Guards are just like guards in function definitions, giving a true or false result.
The variables in the guards are those which appear in generators to the left of the
guard (and any other variables defined at the outer level).
Expression
The expression specifies what the elements of the result will look like.

the left side of the <- arrow doesn’t have to be a variable—it can be any pattern

```
[{area, H * W} || {rectangle, H, W} <- Shapes, H * W >= 10]
```

заменяют map и filter,

опять можно взять примеры с юзерами: get_female, get_id_name

но умеют работать по 2-м и более спискам одновременно

```erlang
10> [ {X,Y} || X <- lists:seq(1,3), Y <- lists:seq(X,3) ].
[{1,1},{1,2},{1,3},{2,2},{2,3},{3,3}]
11> [ {X,Y} || X <- lists:seq(1,4), X rem 2 == 0, Y <- lists:seq(X,4) ].
[{2,2},{2,3},{2,4},{4,4}]
12> [ {X,Y} || X <- lists:seq(1,4), X rem 2 == 0, Y <- lists:seq(X,4), X+Y>4 ].
[{2,3},{2,4},{4,4}]
```

Pythagorean Triplets, armstrong, page 75
Pythagorean triplets are sets of integers {A,B,C} such that A 2 + B 2 = C 2 .
The function pythag(N) generates a list of all integers {A,B,C} such that
A 2 + B 2 = C 2 and where the sum of the sides is less than or equal to N :

```erlang
pythag(N) ->
    [ {A,B,C} ||
    A <- lists:seq(1,N),
    B <- lists:seq(1,N),
    C <- lists:seq(1,N),
    A + B + C =< N,
    A * A + B * B =:= C*C
].
```

```erlang
1> lib_misc:pythag(16).
[{3,4,5},{4,3,5}]
2> lib_misc:pythag(30).
[{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{12,5,13}]
```

Block Expressions, armstrong, page 129
