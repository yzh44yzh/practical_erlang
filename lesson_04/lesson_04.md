# Сопоставление с образцом

Pattern matching

in Erlang is used to:
- Assign values to variables
  оператора присваивания нет, есть оператор сопоставления с образцом :)
- Control the execution flow of programs
- Extract values from compound data types


TODO: прочитать доку
http://erlang.org/doc/reference_manual/expressions.html


## clause

klôz

a unit of grammatical organization next below the sentence in rank and
in traditional grammar said to consist of a subject and predicate.
"In each sentence above, two clauses are linked by clause-chaining without conjunctions."

синонимы: section, paragraph, article, subsection

TODO: немного познакомиться с прологом, в самых общих чертах


## конструкции case и if

TODO: прочитать доку
http://erlang.org/doc/reference_manual/expressions.html#id78374
http://erlang.org/doc/reference_manual/expressions.html#id78310

# guards

TODO: прочитать доку
http://erlang.org/doc/reference_manual/expressions.html#id81911

guards cannot call user-defined functions, since we want to
guarantee that they are side effect free and terminate.

TODO проверить все это:
and, or - вычисляют оба аргумента
andalso, orelse - вычисляют минимум аргументов
comma is andalso, semicolon is orelse


TODO проверить это:
Robert Virding:
Note that there is one very significant difference between ';' and or/orelse and that is how they behave with errors. ';' will just fail that one guard sequence and attempt the next one while or/orelse will fail the guard sequence they are in. So if they are used as an alternative to ';' you will get different behaviour.
That errors in guards cause the guard to fail and not generate an exception was an intentional design decision, it saved a lot of explicit type tests and they were implicit in the operations. For example you could do tuple_size(T) without first having to check if T is a tuple.


====================

TODO: пересмотреть этот старый материал, выбрать из него интересное.

pattern matching

Сопоставление с образцом, мощная синтаксическая конструкция. Используется повсеместно для многих целей.

В Erlang даже нет операции присваивания, а оператор = выполняет pattern matching ).

X = 10

Тут происходит матчинг значения 10 в неопределенную переменную X.

Используется для выборки значения из кортежа или списка (из сложных структур данных):

```
1> User = {user, 42, "Bob"}.
{user,42,"Bob"}
2> {user, Age, _} = User.
{user,42,"Bob"}
3> Age.
42
4> User2 = {user, 77, "Bill"}.
{user,77,"Bill"}
5> Users = [User, User2].
[{user,42,"Bob"},{user,77,"Bill"}]
6> [_, {user, Id, _} | _] = Users.
[{user,42,"Bob"},{user,77,"Bill"}]
7> Id.
77
```

Для реализации условных переходов. Тут несколько вариантов:

Клозы функций

```
1> F = fun({man, Name}) -> "Hello " ++ Name;
1> ({dog, Name}) -> "Hi " ++ Name
1> end.
 #Fun<erl_eval.6.80484245>
2> F({man, "Bob"}).
"Hello Bob"
3> F({dog, "Rex"}).
"Hi Rex"
```

case

```
case Result of
    {ok, Val} -> do_something(Val);
    {error, Error} -> log_error(Error), do_something(DefaultVal)
end.
```

receive

```
receive
    stop -> stop();
    {Pid, Message} -> process(Message), Pid ! received
end.
```

try..catch

```
try
    do_something()
catch
    throw:Error -> log_error(Error);
    error:SystemError -> log_error(SystemError), stop()
end.
```

Вот интересный пример:

```
%% check table owner leave table in waiting state
check_room_owner(RoomId, OwnerId) ->
    case  personal_table:get_table_for_room(RoomId) of
        {ok, #ptable{id = TableId, owner = OwnerId}} ->
            bingo_room_manager:close_table_and_room(TableId, RoomId);
        _ -> do_nothing
    end
end
```

Находим стол, и сразу матчингом UserId проверяем владельца.
case, if, guards

```
case find_user(UserId) of
    {ok, User} -> do_something(User),
                  true;
    {error, not_found} -> false
end
```

```
case find_user(UserId) of
    {ok, #user{age = Age} = User} when Age > 18 -> do_something(User), ok;
    {error, not_found} -> false
end
```

```
if
    User#user.age > 18 -> do_something(User), true;
    User#user.age > 21 -> do_other(User), true;
    true -> false
end
```

if – это как если бы из case убрать вычисление значение и матчинг с образцами, но оставить только гарды
