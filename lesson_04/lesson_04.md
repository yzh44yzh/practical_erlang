## Сопоставление с образцом

Когда я впервые увидел конструкцию **сопоставления с образцом**
(**pattern matching**), я сразу в нее влюблися.  И, может быть, это
главная причина, почему я полюбил эрланг. (На то время я не знал
других функциональных языков). В императивных языках такой конструкции
либо нет вообще, либо есть некое бледное подобие.

Поскольку мы уже пользовались сопоставлением с образцом на предыдущих
уроках, то у вас, наверняка, есть понимание (или интуитивное ощущение)
как это работает. Но теперь пришло время четко разобраться во всех
нюансах.

Итак, сопоставление с образцом используется для:

 - присвоения значений переменным;
 - извлечения значений из сложных структур данных;
 - условных переходов.


### Присвоение значений переменным

```erlang
1> A = 123.
123
```

Даже эта элементарная конструкция, которая выглядит как оператор
присваивания, на самом деле является сопоставлением с образцом. А
оператора присваивания в эрланг нет вообще.

Переменные в эрланг могут быть несвязаные (unbound) и связаные
(bound).  Несвязаная переменная объявлена, но еще не получила никакого
значения.  Связанная переменная уже получила значение, и теперь не
может его изменить.

В данном коде несвязаная переменная **А**, с помощью сопоставления с
образцом получает значение *123*, и становится связаной.


### Извлечение значений из сложных структур данных

```erlang
2> User = {user, "Bob", 25}.
{user,"Bob",25}
3> {user, Name, Age} = User.
{user,"Bob",25}
4> Name.
"Bob"
5> Age.
25
```

Это мы уже делали на предыдущих уроках, сейчас разберем подробнее, что
здесь происходит.  Слева от знака **=** находится шаблон (pattern),
справа значение, которое мы пытаемся сопоставить с шаблоном.

Шаблон может быть любой структурой данных и может содержать
несвязаные и связаные переменные.  Значение справа может быть любой
структурой данных, но может содержать только связанные переменные.

Сопоставление может пройти успешно, и тогда несвязаные переменные в
шаблоне (если они есть), получат свои значения. Или сопоставление
может не пройти, и тогда возникнет исключение -- ошибка времени
выполнения.

```erlang
6> {cat, Name, TailLength} = User.
** exception error: no match of right hand side value {user,"Bob",25}
```

Шаблон может также содержать анонимные переменные (обозначаются
символом подчеркивания), которые совпадают с любым значением.

```erlang
8> {_, Name, _} = User.
{user,"Bob",25}
9> Name.
"Bob"
```

Но их нужно отличать от именованых переменных, чьи имена начинаются
с символа подчеркивания:

```erlang
10> {_Some, Name, _Some} = User.
** exception error: no match of right hand side value {user,"Bob",25}
```

Здесь первое и третье значения кортежа должны быть одинаковыми, чтобы
шаблон совпал.  Но в значении **User** они разные, поэтому получаем
исключение.


### Условные переходы

```erlang
6> case User of
6> {user, _, _} -> "this is user";
6> {cat, _, _} -> "this is cat"
6> end.
"this is user"
```

Сопоставление с образцом также используется в клозах (clause) функций
и в конструкциях **case**, **receive**, **try** для выбора ветки кода,
которая будет выполняться. То есть, для условного перехода.

Ниже мы рассмотрим все эти варианты. А сейчас один пример из реального
проекта. Это игра, где несколько пользователей собираются за одним
столом. Один из игроков является владельцем комнаты.  Данная функция
позволяет определить, является ли данный игрок владельцем данной
комнаты:

```
is_user_owner_of_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} -> true;
        _ -> false
    end.
```

Здесь **rooms:find_room/1** может вернуть либо {ok, #room{}}, либо
{error, not_found}. В первом шаблоне конструкции **case** мы
проверяем, что find_room вернула {ok, #room{}}, причем owner
совпадается с UserId.

Таким образом, мы одним шаблоном проверяем сразу два условия:

 - что комната с таким RoomId существует;
 - что владелец у нее именно UserId, а не кто-то другой.

В императивном языке тут было бы две конструкции **if**.


## clause

Рассмотрим подробнее клозы функции.  Этот термин пишется **clause**,
произносится **[klôz]** и означает одно из нескольких тел функции.

Общепринятого перевода на русский язык нет, поэтому я буду писать без
перевода -- **клоз**, потому что каждый раз писать "одно из нескольких
тел функции", несколько утомительно :)

Примеры мы видели, когда писали рекурсивные функции с аккумуляторами.
Вообще клозов у функции может быть много:

```erlang
area({rect, Width, Height}) -> Width * Height;
area({square, Size}) -> Size * Size;
area({circle, Radius}) -> math:pi() * Radius * Radius.
```

Очередность клозов важна, потому что шаблоны проверяются сверху вниз,
и первое совпадение приводит к выполнению соответствующего клоза.
Поэтому более специфичные шаблоны должны идти раньше, а более общие
поздно. Компилятор может предупредить о неправильной
последовательности шаблонов, но не всегда.

Вот неправильная последовательность шаблонов:

```erlang
case List of
    [] -> empty_list;
    [Head | _] -> process(Head);
    [{X, Y} | _] -> process(X, Y)
end.
```

Шаблон **Head** более общий, чем **{X, Y}**, и третяя вертка кода
никогда не сработает, все перехватит вторая ветка.

Вот правильная последовательность шаблонов:

```erlang
case List of
    [] -> empty_list;
    [{X, Y} | _] -> process(X, Y);
    [Head | _] -> process(Head)
end.
```

## guards

Вообще **guard** переводится как охранник. Но в литературе его не
переводят, а используют англицизм **гард**.

Гарды используются там, где сопоставление с образцом применяется для
условных переходов: то есть, в клозах функций, в case, try и receive
конструкциях.  Они дополняют сопоставление с образцом, позволяя
указать дополнительные условия.

Гадром является последовательность выражений, разделенных запятой,
каждое из которых вычисляется в булевое значение.

```erlang
check_user({user, _, Gender, Age}) when Gender =:= female, Age < 14 -> girl;
check_user({user, _, Gender, Age}) when Gender =:= female, Age >= 14, Age < 21 -> teenage_girl;
check_user({user, _, Gender, Age}) when Gender =:= female, Age >= 21 -> women;
check_user({user, _, Gender, Age}) when Gender =:= male, Age < 14 -> boy;
check_user({user, _, Gender, Age}) when Gender =:= male, Age >= 14, Age < 21 -> teenage_boy;
check_user({user, _, Gender, Age}) when Gender =:= male, Age >= 21 -> men.
```

Гард срабатывает (разрешает выполнение данной ветки кода), если все
выражения вычисляются в true.

Гарды могут объединяться в последовательности, разделенные точкой с запятой:

```erlang
check_user({user, _, Gender, Age})
  when Gender =:= female, Age < 14;
       Gender =:= male, Age < 14
       -> child;
check_user({user, _, Gender, Age})
  when Gender =:= male, Age >= 21;
       Gender =:= male, Age >= 21
       -> adult.
```

Последовательность гардов срабатывает, если срабатывает любой из
гардов в ней.

То есть, запятая работает как **andalso**, а точка с запятой работает
как **orelse** и код выше эквивалентен:

```erlang
check_user({user, _, Gender, Age})
  when (Gender =:= female andalso Age < 14) orelse
       (Gender =:= male andalso Age < 14)
       -> child;
check_user({user, _, Gender, Age})
  when (Gender =:= male andalso Age >= 21) orelse
       (Gender =:= male andalso Age >= 21)
       -> adult.
```

Я предпочитаю второй вариант -- явное использование andalso и orelse.
Он длиннее, зато не требует от всех, читающих код, помнить, что значит
запятая, а что точка с запятой :)

Выражения в гардах не должны иметь побочных эффектов. Поэтому
разрешены не любые эрганговские выражения, а только их
подмножество. Например, запрещен вызов пользовательских функций. Да и
встроенные функции можно вызывать не все.  Что именно разрешено,
[смотрите в документации](http://erlang.org/doc/reference_manual/expressions.html#id81911)



f(X) when (X == 0) or (1/X > 2) ->
...
g(X) when (X == 0) orelse (1/X > 2) ->
...
The guard in f(X) fails when X is zero but succeeds in g(X).
In practice, few programs use complex guards, and simple ( , ) guards
suffice for most programs.

TODO проверить все это:
and, or - вычисляют оба аргумента
andalso, orelse - вычисляют минимум аргументов
comma is andalso, semicolon is orelse


TODO проверить это:
Robert Virding:
Note that there is one very significant difference between ';' and or/orelse and that is how they behave with errors. ';' will just fail that one guard sequence and attempt the next one while or/orelse will fail the guard sequence they are in. So if they are used as an alternative to ';' you will get different behaviour.
That errors in guards cause the guard to fail and not generate an exception was an intentional design decision, it saved a lot of explicit type tests and they were implicit in the operations. For example you could do tuple_size(T) without first having to check if T is a tuple.


## конструкция case

case Expr of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end

The expression Expr is evaluated and the patterns Pattern are sequentially matched against the result. If a match succeeds and the optional guard sequence GuardSeq is true, the corresponding Body is evaluated.

If there is no matching pattern with a true guard sequence, a case_clause run-time error will occur.

The scope for a variable is its function clause. Variables bound in a branch of an if, case, or receive expression must be bound in all branches to have a value outside the expression, otherwise they will be regarded as 'unsafe' outside the expression.


## конструкция if

if
    GuardSeq1 ->
        Body1;
    ...;
    GuardSeqN ->
        BodyN
end

The branches of an if-expression are scanned sequentially until a guard sequence GuardSeq which evaluates to true is found. Then the corresponding Body (sequence of expressions separated by ',') is evaluated.

If no guard sequence is true, an if_clause run-time error will occur. If necessary, the guard expression true can be used in the last branch, as that guard sequence is always true.
