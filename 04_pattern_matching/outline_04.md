## Сопоставление с образцом, гарды.

сопоставление с образцом **pattern matching**

В императивных языках такой конструкции либо нет вообще, либо есть некое бледное подобие.

Используется для:

 - присвоения значений переменным;
 - извлечения значений из сложных структур данных;
 - условных переходов.


### Присвоение значений переменным

```erlang
1> A = 123.
```

Выглядит как оператор присваивания, на самом деле является сопоставлением с образцом.

Переменные несвязанные (unbound) и связанные (bound).

Несвязанная переменная **А**, с помощью сопоставления с образцом получает значение *123*, и становится связанной.


### Извлечение значений из сложных структур данных

```erlang
2> User = {user, "Bob", 25}.
3> {user, Name, Age} = User.
4> Name.
"Bob"
5> Age.
25
```

Слева от знака **=** находится шаблон (pattern).
Справа значение, которое мы пытаемся сопоставить с шаблоном.

Шаблон может быть любой структурой данных и может содержать
несвязанные и связанные переменные.

Значение справа может быть любой структурой данных, но может содержать
только связанные переменные.

Сопоставление может пройти успешно.

Может не пройти, и тогда генерируется исключение.


```erlang
6> {cat, Name, TailLength} = User.
** exception error: no match of right hand side value {user,"Bob",25}
```

Анонимные переменные, которые совпадают с любым значением.

```erlang
8> {_, Name, _} = User.
{user,"Bob",25}
9> Name.
"Bob"
```

Отличать от именованных переменных, чьи имена начинаются с символа подчеркивания:

```erlang
10> {_Some, Name, _Some} = User.
** exception error: no match of right hand side value {user,"Bob",25}
```

### Условные переходы

```erlang
6> case User of
6> {user, _, _} -> "this is user";
6> {cat, _, _} -> "this is cat"
6> end.
"this is user"
```

клозы (clause) функций
конструкциях **case**, **receive**, **try**

```erlang
is_user_owner_of_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} -> true;
        _ -> false
    end.
```

Одним шаблоном проверяем сразу два условия:

 - что комната с таким RoomId существует;
 - что владелец у нее именно UserId, а не кто-то другой.


## clause

Пишется **clause**, произносится **[klôz]** и означает одно из нескольких тел функции.

Клозов у функции может быть много:

```erlang
area({rect, Width, Height}) -> Width * Height;
area({square, Size}) -> Size * Size;
area({circle, Radius}) -> math:pi() * Radius * Radius.
```

Очередность клозов важна, потому что шаблоны проверяются сверху вниз.

Вот неправильная последовательность шаблонов:

```erlang
case List of
    [] -> empty_list;
    [Head | _] -> process(Head);
    [{X, Y} | _] -> process(X, Y)
end.
```

Вот правильная последовательность шаблонов:

```erlang
case List of
    [] -> empty_list;
    [{X, Y} | _] -> process(X, Y);
    [Head | _] -> process(Head)
end.
```

## guards

**guard** переводится как "охранник".
Не переводят, а используют англицизм **гард**.

Они дополняют сопоставление с образцом, позволяя указать дополнительные условия.

Последовательность выражений, разделенных запятой,
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

Запятая работает как **andalso**,
а точка с запятой работает как **orelse**.

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

Выражения в гардах не должны иметь побочных эффектов.

Разрешено подмножество выражений
http://erlang.org/doc/reference_manual/expressions.html#id81911

Гард проглатывает исключение

```erlang
1> F = fun(X) when 5/X > 1 -> "clause 1";
1> (X) -> "clause 2"
1> end.
 #Fun<erl_eval.6.90072148>
2> F(0).
"clause 2"
3> F(2).
"clause 1"
```

Поэтому можно не писать так:

```erlang
1> F = fun(X) when is_tuple(X), tuple_size(X) == 2 -> X end.
```

а писать сразу так:

```erlang
2> F = fun(X) when tuple_size(X) == 2 -> X end.
```

## конструкция case

Аналогична клозам функции, но может использоваться в любом месте в коде.

```erlang
case Expr of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end
```

Шаблоны применяются к выражению по очереди, сверху вниз.

Если ни один шаблон не совпал, то генерируется исключение.

case могут быть вложенными друг в друга.

```erlang
close_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} ->
            case rooms:close(RoomId) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {ok, #room{}} -> {error, not_room_owner};
        {error, not_found} -> {error, room_not_found}
    end.
```

Unsafe переменная.

Не использовать переменную за пределами ветки case, в которой она
объявлена, либо объявить во всех ветках.


## конструкция if

Упрощенный **case** без выражения и без шаблонов, а ветки представлены только гардами.

```erlang
if
    GuardSeq1 ->
        Body1;
    ...;
    GuardSeqN ->
        BodyN
end
```

Если ни один гард не сработал, генерируется исключение.

Часто последним гардом ставят true, и он срабатывает всегда.

```erlang
valid_char(Char) ->
    IsDigit = is_digit(Char),
    IsAlpha = is_alpha(Char),
    if
        IsDigit -> true;
        IsAlpha -> true;
        true -> false
    end.
```

Видим, как обойти ограничение на использование своих функций в гардах.

catch all pattern, для case так делают реже, чем в случае с if.
